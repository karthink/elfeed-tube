;;; elfeed-tube-fill.el --- Back-fill elfeed-tube feeds  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: multimedia, convenience

;; SPDX-License-Identifier: UNLICENSE

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; This file contains commands to back-fill Elfeed YouTube feeds. Back-filling a
;; feed fetches all entries for the corresponding YouTube channel or playlist
;; and adds them to the Elfeed database. Youtube RSS feeds generally contain
;; only the latest 15 entries.
;;
;; Call `elfeed-tube-fill-feeds' in an Elfeed search or entry buffer to
;; back-fill entries for the corresponding feed.
;;
;;; Code:

(require 'elfeed-tube)

(declare-function elfeed-tube--get-entries "elfeed-tube")
(defvar elfeed-tube--api-channels-videos-path "/api/v1/channels/videos/%s")
(defvar elfeed-tube--api-playlists-videos-path "/api/v1/playlists/%s")

;;;###autoload (autoload 'elfeed-tube-fill-feeds "elfeed-tube-utils" "Fetch and add all channel videos for ENTRIES' feeds." t nil)
(aio-defun elfeed-tube-fill-feeds (entries &optional interactive-p)
  "Fetch and add all channel videos for ENTRIES' feeds.

YouTube RSS feeds generally contain only the latest 15 entries.
Use this command to fetch and add to Elfeed all videos
corresponding a channel or playlist.

ENTRIES is the entry at point or visited entry, or the list of
selected entries if the region is active.

When called interactively, INTERACTIVE-P is t and a summary
window will be shown before taking any action."
  (interactive (list (elfeed-tube--ensure-list (elfeed-tube--get-entries))
                     t))
  (let ((feeds (cl-reduce
                (lambda (accum entry)
                  (if-let* ((feed (elfeed-entry-feed entry))
                            ((memq feed accum)))
                      accum
                    (cons feed accum)))
                entries
                :initial-value nil)))
    (if interactive-p
        (elfeed-tube--fill-display-feeds feeds)
      (aio-await (elfeed-tube--fill-feeds feeds)))))

(aio-defun elfeed-tube-fill--confirm ()
  "Back-fill video entries for the displayed Elfeed feeds."
  (interactive)
  (cl-assert (derived-mode-p 'elfeed-tube-channels-mode))
  (cl-loop for table-entry in tabulated-list-entries
           for feed-title = (car (aref (cadr table-entry) 0))
           collect (get-text-property 0 'feed feed-title) into feeds
           finally do (elfeed-tube-log 'debug "[fill-confirm-feeds: %S]"
                                       (mapcar #'elfeed-feed-title feeds))
           finally do
           (progn
             (quit-window 'kill-buffer)
             (message "Backfilling YouTube feeds...")
             (aio-await (elfeed-tube--fill-feeds feeds))
             (message "Backfilling Youtube feeds... done."))))

(defun elfeed-tube--fill-display-feeds (feeds)
  "Produce a summary of Elfeed FEEDS to be back-filled.

Back-filling a YouTube feed will fetch all its videos not
presently available in its RSS feed or in the Elfeed database."
  (let ((buffer (get-buffer-create "*Elfeed-Tube Channels*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (elfeed-tube-channels-mode)

      (setq tabulated-list-use-header-line t ; default to no header
            header-line-format nil
            ;; tabulated-list--header-string nil
            tabulated-list-format
            '[("Channel" 22 t)
              ("#Entries" 10 t)
              ("Feed URL" 30 nil)])

      (setq
       tabulated-list-entries
       (cl-loop for feed in feeds
                for n upfrom 1
                for feed-url = (elfeed-feed-url feed)
                for channel-id = (progn (string-match "=\\(.*?\\)$" feed-url)
                                        (match-string 1 feed-url))
                for feed-title = (list (propertize (elfeed-feed-title feed)
                                                   'feed feed)
                                       'mouse-face 'highlight
                                       'action
                                       #'elfeed-tube-add--visit-channel
                                       'follow-link t
                                       'help-echo
                                       (or (and channel-id
                                                (concat
                                                 "https://www.youtube.com/channel/"
                                                 channel-id))
                                           ""))
                for feed-count = (number-to-string (length (elfeed-feed-entries feed)))
                collect
                `(,n
                  [,feed-title
                   ,feed-count
                   ,feed-url])))

      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-address-mode 1)

      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (continue (propertize "C-c C-c" 'face 'help-key-binding))
            (cancel-q (propertize "q" 'face 'help-key-binding))
            (cancel   (propertize "C-c C-k" 'face 'help-key-binding)))

        (let ((inhibit-message t)) (toggle-truncate-lines 1))
        (insert "\n")
        (insert
           ;; (propertize
           ;;  "All queries resolved successfully.\n\n"
           ;;  'face 'success)
           "     " continue ": Add ALL videos from these channels to Elfeed.\n")
        (insert cancel-q " or " cancel ": Quit and cancel this operation.\n"))

      (goto-char (point-min))

      (use-local-map (copy-keymap elfeed-tube-channels-mode-map))
      (local-set-key (kbd "C-c C-c") #'elfeed-tube-fill--confirm)

      (display-buffer
       buffer `(nil
                (window-height . ,#'fit-window-to-buffer)
                (body-function . ,#'select-window))))))

(aio-defun elfeed-tube--fill-feeds (feeds)
  "Find videos corresponding to the channels/playlists for Elfeed feeds FEEDS.

Videos not already present will be added to the Elfeed database."
  (cl-assert (not (null feeds)))
  (cl-assert (listp feeds))
  (cl-assert (elfeed-feed-p (car feeds)))

  (dolist (feed feeds)
    (let ((elfeed-tube-auto-fetch-p nil)
          (feed-url (elfeed-feed-url feed))
          (feed-id  (elfeed-feed-id feed))
          (feed-title (elfeed-feed-title feed))
          (add-count)
          (feed-entries-to-add
           (thread-first
             (elfeed-tube--fill-feed feed)
             (aio-await)
             (cl-delete-duplicates :key (lambda (x) (plist-get x :videoId)) :test #'string=)
             (vconcat)
             (elfeed-tube--fill-feed-dates)
             (aio-await))))

      (elfeed-tube-log 'debug "[(fill-feeds): Backfilling feed: %s]" feed-title)
      (cl-assert (vectorp feed-entries-to-add))

      (if (= (length feed-entries-to-add) 0)
          (message "Nothing to retrieve for feed \"%s\" (%s)" feed-title feed-url)
        (cl-assert (stringp (plist-get (aref feed-entries-to-add 0) :videoId)))
        (cl-assert (integerp (plist-get (aref feed-entries-to-add 0) :published)))
        (cl-assert (stringp (plist-get (aref feed-entries-to-add 0) :title)))
        (setq add-count (length feed-entries-to-add))

        (condition-case error
            (thread-last
              feed-entries-to-add
              (cl-map 'list (apply-partially #'elfeed-tube--entry-create feed-id))
              (elfeed-db-add))
          (error (elfeed-handle-parse-error feed-url error)))
        ;; (prin1 feed-entries-to-add (get-buffer "*scratch*"))
        (elfeed-tube-log 'debug "[(elfeed-db): Backfilling feed: %s][Added %d videos]"
                         feed-title add-count)
        (message "Retrieved %d missing videos for feed \"%s\" (%s)"
                 add-count feed-title feed-url)
        (run-hook-with-args 'elfeed-update-hooks feed-url))))
  (elfeed-search-update :force))

;; feed: elfeed-feed struct, page: int or nil -> vector(plist entries for feed videos not in db)
(aio-defun elfeed-tube--fill-feed (feed &optional page)
  "Find videos corresponding to the channel/playlist for Elfeed feed FEED.

Return video metadata as a vector of plists. Metadata
corresponding to videos already in the Elfeed database are
filtered out.

PAGE corresponds to the page number of results requested from the API."
  (cl-assert (elfeed-feed-p feed))
  (cl-assert (or (null page) (integerp page)))

  (if-let* ((page (or page 1))
            (feed-url (elfeed-feed-url feed))
            (feed-title (elfeed-feed-title feed))
            (api-path (cond ((string-match "playlist_id=\\(.*?\\)/*$" feed-url)
                             (concat
                              (format elfeed-tube--api-playlists-videos-path
                                      (match-string 1 feed-url))
                              "?fields=videos(title,videoId,author)"
                              "&page=" (number-to-string (or page 1))))
                            ((string-match "channel_id=\\(.*?\\)/*$" feed-url)
                             (concat
                              (format elfeed-tube--api-videos-path
                                      (match-string 1 feed-url))
                              "?fields="
                              "title,videoId,author,published"
                              "&sort_by=newest"
                              "&page=" (number-to-string (or page 1))))
                            (t (elfeed-tube-log 'error "[Malformed/Not YouTube feed: %s][%s]"
                                                feed-title feed-url)
                               nil)))
            (feed-type (cond ((string-match "playlist_id=\\(.*?\\)/*$" feed-url) 'playlist)
                             ((string-match "channel_id=\\(.*?\\)/*$" feed-url)  'channel))))
      (let ((feed-entry-video-ids
             (mapcar (lambda (e) (elfeed-tube--url-video-id (elfeed-entry-link e)))
                     (elfeed-feed-entries feed)))
            (feed-id (elfeed-feed-id feed)))
        (if-let*
            ((api-data
              (aio-await
               (elfeed-tube--aio-fetch
                (concat (aio-await (elfeed-tube--get-invidious-url)) api-path)
                #'elfeed-tube--nrotate-invidious-servers)))
             (api-data (pcase feed-type
                         ('channel
                          (cl-assert (vectorp api-data))
                          (cl-assert (and (plist-get (aref api-data 0) :title)
                                          (plist-get (aref api-data 0) :videoId)
                                          (plist-get (aref api-data 0) :published)))
                          api-data)
                         ('playlist
                          (cl-assert (and (not (null api-data)) (listp api-data)))
                          (cl-assert (vectorp (plist-get api-data :videos)))
                          (plist-get api-data :videos))))
             ((> (length api-data) 0)))
            (progn
              (elfeed-tube-log 'debug "[Backfilling: page %d][Fetched: %d entries]"
                               (or page 1) (length api-data))
              (vconcat
               (cl-delete-if   ;remove entries already in db
                (lambda (elt) (member (plist-get elt :videoId) feed-entry-video-ids))
                api-data)
               (aio-await (elfeed-tube--fill-feed feed (1+ page)))))
          (make-vector 0 0)))
    (elfeed-tube-log 'error "[No channel ID found][%s][%s]" feed-title feed-url)))

;; api-data: vector(plist entries for feed videos) -> vector(plist entries for
;; feed videos with correct dates.)
(aio-defun elfeed-tube--fill-feed-dates (api-data)
  "Add or correct dates for videos in API-DATA.

API-DATA is a vector of plists, one per video. This function
returns a vector of plists with video publish dates
corrected/added as the value of the plist's :published key."
  (let ((date-queries (aio-make-select))
        (feed-videos-map (make-hash-table :test 'equal)))

    (if (= (length api-data) 0)
        api-data
      (progn
        (cl-loop for video-plist across api-data
                 for video-id = (plist-get video-plist :videoId)
                 do (puthash video-id video-plist feed-videos-map)
                 do (aio-await (aio-sleep 0.2 nil))
                 do (aio-select-add
                     date-queries
                     (elfeed-tube--with-label
                      video-id #'elfeed-tube--aio-fetch
                      (concat (aio-await (elfeed-tube--get-invidious-url))
                              elfeed-tube--api-videos-path
                              video-id "?fields=published"))))

        (while (aio-select-promises date-queries)
          (pcase-let* ((`(,video-id . ,corrected-date)
                        (aio-await (aio-await (aio-select date-queries))))
                       (video-plist (gethash video-id feed-videos-map))

                       (old-timestamp (plist-get video-plist :published))
                       (new-timestamp))

            (plist-put video-plist :published (plist-get corrected-date :published))

            (setq new-timestamp (plist-get corrected-date :published))
            (elfeed-tube-log 'debug "[video-id: %S][fix date: %S → %S]"
                             video-id old-timestamp new-timestamp)))

        (vconcat (hash-table-values feed-videos-map))))))

(provide 'elfeed-tube-fill)
;;; elfeed-tube-fill.el ends here

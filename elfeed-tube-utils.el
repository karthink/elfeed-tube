;;; elfeed-tube-utils.el --- utilities for elfeed-tube  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: multimedia, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'rx)
(require 'aio)

(defvar elfeed-tube--yt-base-url
  (rx bol
      (zero-or-one (or "http://" "https://"))
      (zero-or-one "www.")
      (or "youtube.com/" "youtu.be/")))

(cl-defstruct (elfeed-tube-channel (:constructor elfeed-tube-channel-create)
                                   (:copier nil))
  "Struct to hold youtube channel information."
  query author url feed)

;;;###autoload (autoload 'elfeed-tube-add-feeds "elfeed-tube-utils" "Add youtube feeds to the Elfeed database by QUERIES." t nil)
(aio-defun elfeed-tube-add-feeds (queries &optional save)
  "Add youtube feeds to the Elfeed database by QUERIES.

Each query can be a video, playlist or channel URL and the
corresponding channel feed will be selected. It can also be a
search term and the best match will be found. You will be asked
to finalize the results before committing them to Elfeed.

When called interactively, multiple queries can be provided by
separating them with the `crm-separator', typically
comma (\",\"). Search terms cannot include the `crm-separator'.

When called noninteractively, it accepts a query or a list of
queries."
  (interactive
   (list (completing-read-multiple
          "Video, Channel, Playlist URLs or search queries: "
          #'ignore)
         current-prefix-arg))
  (message "Finding RSS feeds, hold tight!")
  (let ((channels (aio-await (elfeed-tube-add--get-channels queries))))
    (elfeed-tube-add--display-channels channels)))

(aio-defun elfeed-tube-add--get-channels (queries)
  (let* ((queries (ensure-list queries))
         (playlist-base-url
          "https://www.youtube.com/feeds/videos.xml?playlist_id=")
         (channel-base-url
          "https://www.youtube.com/feeds/videos.xml?channel_id=")
         channels)
    (dolist (q queries channels)
      (setq q (string-trim q))
      (cond
       ((elfeed-tube--channel-p q)
        (if-let* ((chan-id (match-string 1 q))
                  (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                   "/api/v1/channels/"
                                   chan-id
                                   "?fields=author,authorUrl"))
                  (data (aio-await (elfeed-tube--aio-fetch
                                    api-url #'elfeed-tube--nrotate-invidious-servers)))
                  (author (plist-get data :author))
                  (author-url (plist-get data :authorUrl))
                  (feed (concat channel-base-url chan-id)))
            (push (elfeed-tube-channel-create
                   :query q :author author
                   :url  q
                   :feed feed)
                  channels)
          (push (elfeed-tube-channel-create :query q :feed feed)
                channels)))

       ((string-match
         (concat elfeed-tube--yt-base-url "c/" "\\([^?&]+\\)") q)
        (let ((chan (car (aio-await
                          (elfeed-tube-add--get-channels (match-string 1 q))))))
          (setf (elfeed-tube-channel-query chan) q)
          (push chan channels)))

       ((elfeed-tube--playlist-p q)
        (if-let* ((playlist-id (match-string 1 q))
                  (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                   "/api/v1/playlists/"
                                   playlist-id
                                   "?fields=title,author"))
                  (data (aio-await (elfeed-tube--aio-fetch
                                    api-url #'elfeed-tube--nrotate-invidious-servers)))
                  (title (plist-get data :title))
                  (author (plist-get data :author))
                  (feed (concat playlist-base-url playlist-id)))
            (push (elfeed-tube-channel-create
                   :query q :author title :url q
                   :feed feed)
                  channels)
          (push (elfeed-tube-channel-create
                 :query q :url q
                 :feed (concat playlist-base-url playlist-id))
                channels)))

       ((elfeed-tube--video-p q)
        (if-let* ((video-id (match-string 1 q))
                  (videos-url "/api/v1/videos/")
                  (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                   videos-url
                                   video-id
                                   "?fields=author,authorUrl,authorId"))
                  (data (aio-await (elfeed-tube--aio-fetch
                                    api-url #'elfeed-tube--nrotate-invidious-servers)))
                  (author (plist-get data :author))
                  (author-id (plist-get data :authorId))
                  (author-url (plist-get data :authorUrl))
                  (feed (concat channel-base-url author-id)))
            (push (elfeed-tube-channel-create
                   :query q :author author
                   :url (concat "https://www.youtube.com" author-url)
                   :feed feed)
                  channels)
          (push (elfeed-tube-channel-create :query q)
                channels)))

       (t ;interpret as search query
        (if-let* ((search-url "/api/v1/search")
                  (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                   search-url
                                   "?q=" (url-hexify-string q)
                                   "&type=channel&page=1"))
                  (data (aio-await (elfeed-tube--aio-fetch
                                    api-url #'elfeed-tube--nrotate-invidious-servers)))
                  (chan-1 (and (> (length data) 0)
                               (aref data 0)))
                  (author (plist-get chan-1 :author))
                  (author-id (plist-get chan-1 :authorId))
                  (author-url (plist-get chan-1 :authorUrl))
                  (feed (concat channel-base-url author-id)))
            (push (elfeed-tube-channel-create
                   :query q :author author
                   :url (concat "https://www.youtube.com" author-url)
                   :feed feed)
                  channels)
          (push (elfeed-tube-channel-create :query q)
                channels)))))
    (nreverse channels)))

(defun elfeed-tube-add--display-channels (channels)
  (defsubst prophelp (s)
    (propertize s 'face 'help-key-binding))
  (let ((buffer (get-buffer-create "*Elfeed-Tube Channels*"))
        (notfound (propertize "Not found!" 'face 'error)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (elfeed-tube-channels-mode)
      (setq
       tabulated-list-entries
       (cl-loop for channel in channels
                for n upfrom 1
                for author = (or (elfeed-tube-channel-url channel) notfound)
                for feed = (or (elfeed-tube-channel-feed channel) notfound)
                collect
                `(,n
                  [,(or (and
                         (elfeed-tube-channel-author channel)
                         (list (elfeed-tube-channel-author channel)
                               'mouse-face 'highlight
                               'action
                               #'elfeed-tube-add--visit-channel
                               'follow-link t
                               'help-echo (elfeed-tube-channel-url channel)))
                        notfound)
                   ,(replace-regexp-in-string
                     elfeed-tube--yt-base-url ""
                     (elfeed-tube-channel-query channel))
                   ,(or (elfeed-tube-channel-feed channel)
                        (propertize "Not found!" 'face 'error))])))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-address-mode 1)
      
      (goto-char (point-max))
      
      (let ((inhibit-read-only t)
            (fails (cl-reduce
                    (lambda (sum ch)
                      (+ sum
                         (or (and (elfeed-tube-channel-feed ch) 0) 1)))
                    channels :initial-value 0))
            (continue (propertize "C-c C-c" 'face 'help-key-binding))
            (continue-extra (prophelp "C-u C-c C-c"))
            (cancel-q (propertize "q" 'face 'help-key-binding))
            (cancel   (propertize "C-c C-k" 'face 'help-key-binding))
            (copy     (propertize "C-c C-w" 'face 'help-key-binding)))
        
        (toggle-truncate-lines 1)
        (insert "\n")
        (when (> fails 0)
          (insert (propertize
                   (format "%d queries could not be resolved.\n\n" fails)
                   'face 'error)
                  "     " continue ": Add found feeds to the Elfeed database, ignoring the failures.\n"
                  " " continue-extra ": Add found feeds, fetch entries from them and open Elfeed.\n"))
        (when (= fails 0)
          (insert
           (propertize
            "All queries resolved successfully.\n\n"
            'face 'success)
           "     " continue ": Add all feeds to the Elfeed database.\n"
           " " continue-extra ": Add all feeds, fetch entries from them and open Elfeed.\n"
           "     " copy ": Copy the list of feed URLs as a list\n"))
        (insert "\n" cancel-q " or " cancel ": Quit and cancel this operation."))
      
      (goto-char (point-min))
      
      ;; (let ((window (display-buffer buffer)))
      ;;   ;; (set-window-dedicated-p window t)
      ;;   buffer)
      
      (let ((window
             (funcall
              (if (bound-and-true-p demo-mode)
                  #'switch-to-buffer
                #'display-buffer)
              buffer)))
        buffer)
      
      )))

(defun elfeed-tube-add--visit-channel (button)
  (browse-url (button-get button 'help-echo)))

;; (elfeed-tube-add--display-channels my-channels)

(defun elfeed-tube-add--confirm (&optional arg)
  "Confirm the addition of visible Youtube feeds to the Elfeed
database.

With optional prefix argument ARG, update these feeds and open Elfeed
afterwards."
  (interactive "P")
  (cl-assert (derived-mode-p 'elfeed-tube-channels-mode))
  (let* ((channels tabulated-list-entries)
         authors feeds)
    (message "Added to elfeed-feeds.")
    (cl-loop for channel in channels
             for (author query feed) = (append (cadr channel) nil)
             do (elfeed-add-feed feed :save t))
    (when arg (elfeed))))

(define-derived-mode elfeed-tube-channels-mode tabulated-list-mode
  "Elfeed Tube Channels"
  (setq tabulated-list-use-header-line t ; default to no header
        header-line-format nil
        ;; tabulated-list--header-string nil
        tabulated-list-format
        '[("Channel" 22 t)
          ("Query" 32 t)
          ("Feed URL" 30 nil)]))

(define-key elfeed-tube-channels-mode-map (kbd "C-c C-k") #'kill-buffer)
(define-key elfeed-tube-channels-mode-map (kbd "C-c C-c") #'elfeed-tube-add--confirm)
(define-key elfeed-tube-channels-mode-map (kbd "C-c C-w") #'elfeed-tube-add--copy)

(defun elfeed-tube-add--copy ()
  "Copy visible Youtube feeds to the kill ring as a list.

With optional prefix argument ARG, update these feeds and open Elfeed
afterwards."
  (interactive)
  (cl-assert (derived-mode-p 'elfeed-tube-channels-mode))
  (let* ((channels tabulated-list-entries)
         authors feeds)
    (cl-loop for channel in channels
             for (author query feed) = (append (cadr channel) nil)
             collect feed into feeds
             finally (kill-new (prin1-to-string feeds)))
    (message "Feed URLs saved to kill-ring.")))

;; (defvar elfeed-tube-channels-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c C-k") #'kill-buffer)
;;     (define-key map (kbd "C-c C-c") #'elfeed-tube-add--confirm)
;;     map
;;     ;; (make-composed-keymap (list map) tabulated-list-mode-map)
;;     ))

(aio-defun elfeed-tube--aio-fetch (url &optional next desc attempts)
  "Fetch URL asynchronously using `elfeed-curl-retrieve'.

If successful (HTTP 200), return the JSON-parsed result as a
plist.

Otherwise, call the function NEXT (with no arguments) and try
ATTEMPTS more times. Return nil if all attempts fail. DESC is a
description string to print to the elfeed-tube log allong with
any other error messages.

This function returns a promise.
"
  (let ((attempts (or attempts (1+ elfeed-tube--max-retries))))
    (when (> attempts 0)
      (let* ((response
              (aio-await (elfeed-tube-curl-enqueue url :method "GET")))
             (content (plist-get response :content))
             (status (plist-get response :status-code))
             (error-msg (plist-get response :error-message)))
        (cond
         ((= status 200)
          (condition-case nil
              (json-parse-string content :object-type 'plist)
            ('(json-parse-error error)
             (elfeed-tube-log 'error "[Search] JSON malformed (%s)"
                              (elfeed-tube--attempt-log attempts))
             (and (functionp next) (funcall next))
             (aio-await
              (elfeed-tube--search-to-chan url next desc (1- attempts))))))
         (t (elfeed-tube-log 'error "[Search][%s]: %s (%s)" error-msg url
                             (elfeed-tube--attempt-log attempts))
            (and (functionp next) (funcall next))
            (aio-await
             (elfeed-tube--search-to-chan url next desc (1- attempts)))))))))

(defsubst elfeed-tube--video-p (cand)
  (string-match
   (concat
    elfeed-tube--yt-base-url
    (rx (zero-or-one "watch?v=")
        (group (1+ (not "&")))))
   cand))

(defsubst elfeed-tube--playlist-p (cand)
  (string-match
   (concat
    elfeed-tube--yt-base-url
    "playlist\\?list="
    (rx (group (1+ (not "&")))))
   cand))

(defsubst elfeed-tube--channel-p (cand)
  (string-match
   (concat
    elfeed-tube--yt-base-url
    (rx "channel/"
        (group (1+ (not "&")))))
   cand))

(provide 'elfeed-tube-utils)
;;; elfeed-tube-utils.el ends here

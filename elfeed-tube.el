;;; elfeed-tube.el --- Fetch metadata from Invidious for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.45
;; Package-Requires: ((emacs "26.1") (request "0.3.3"))
;; Keywords: news, convenience, multimedia
;; URL: https://github.com/karthink/elfeed-tube

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

(require 'elfeed)
(require 'request)
(require 'cl-lib)
(require 'subr-x)

(defgroup elfeed-tube nil
  "Elfeed-tube: View youtube details in Elfeed"
  :group 'elfeed
  :prefix "elfeed-tube-")

(defcustom elfeed-tube-thumbnail-size 'small
  "Video thumbnail size to show in the Elfeed buffer.

Choices are LARGE, MEDIUM and SMALL.
Set this to NIL to disable showing thumbnails."
  :group 'elfeed-tube
  :type '(choice (const :tag "No thumbnails" nil)
                 (const :tag "Large thumbnails" large)
                 (const :tag "Medium thumbnails" medium)
                 (const :tag "Small thumbnails" small)))

(defcustom elfeed-tube-invidious-url nil
  "Invidious URL to use for retrieving data.

Setting this is optional: If left unset, elfeed-tube will locate
and use an Invidious URL at random. This should be set to a
string, for example \"https://invidio.us\". "
  :group 'elfeed-tube
  :type '(choice (string :tag "Custom URL")
                 (const :tag "Disabled (Auto)" nil)))

(defcustom elfeed-tube-youtube-regexps '("youtube\\.com" "youtu\\.be")
  "List of regular expressions to match Elfeed entry URLs against.

Only entries that match one of these regexps will be handled by
elfeed-tube when fetching information."
  :group 'elfeed-tube
  :type '(repeat string))

(defvar elfeed-tube--debug t)
(defvar elfeed-tube-add-duration t)
(defvar elfeed-tube--api-videos-path "/api/v1/videos/")
(defvar elfeed-tube--info-table (make-hash-table :test #'equal))
(defvar elfeed-tube--invidious-servers nil)

(defmacro elfeed-tube--defcallback (name args &rest body)
  "Create a callback function NAME for a web request.

ARGS is a list of parameters used by NAME that are passed as part
of the request response. The first element of ARGS should always
be DATA, the response data. BODY is the body of the callback
function."
  (declare (indent defun))
  `(cl-defun ,name (&key (data nil)
                         (error-thrown nil)
                         (symbol-status nil)
                         (response nil)
                         &allow-other-keys)
     (let* ((settings (request-response-settings response))
            ,@(mapcar (lambda (s) `(,s (plist-get settings
                                             ,(elfeed-tube--keywordize s))))
                      (or (cdr-safe args) '(symbol-status))))
       ,@body)))

(defsubst elfeed-tube--keywordize (s)
  (intern (concat ":" (symbol-name s))))

(defun elfeed-tube--youtube-p (entry)
  (cl-some (lambda (regex) (string-match-p regex (elfeed-entry-link entry)))
           elfeed-tube-youtube-regexps))

(defsubst elfeed-tube--get-video-id (entry)
  (when (elfeed-tube--youtube-p entry)
    (thread-first (elfeed-entry-id entry)
                  cdr-safe
                  (substring 9))))

(defun elfeed-tube--get-invidious-url ()
  (or (and (not (string= "" elfeed-tube-invidious-url))
           elfeed-tube-invidious-url)
      (and elfeed-tube--invidious-servers
           (elt elfeed-tube--invidious-servers
                (cl-random (length elfeed-tube--invidious-servers))))
      "https://invidio.us"))

(defun elfeed-tube--fetch-maybe (entry &optional db-insert-p attempts)
  (when-let*
      ((attempts (or attempts 3))
       (video-id (elfeed-tube--get-video-id entry))
       (api-url (concat
                 (elfeed-tube--get-invidious-url)
                 elfeed-tube--api-videos-path
                 video-id)))
    (let ((content (gethash video-id elfeed-tube--info-table)))
      (cond
       ((and content (equal db-insert-p '(4)))
        (elfeed-tube--write-db entry content)
        (message "Updated the database for '%s'" (elfeed-entry-title entry)))
       ((and content (not (equal db-insert-p '(16))))
        ;; (message
        ;;  "Info for entry already fetched. Press 'C-u C-u %s' to force refresh."
        ;;  (this-command-keys))
        (elfeed-tube-show))
       ((> attempts 0)
        (when elfeed-tube--debug
          (message "Attempting to access %s" api-url))
        (request api-url
        :type "GET"
        :params
        '(("fields" . "videoThumbnails,descriptionHtml,lengthSeconds"))
        :parser (lambda ()
                  (let ((json-object-type (quote plist)))
                    (json-read)))
        :attempts attempts
        :entry entry
        :db-insert-p db-insert-p
        :success #'elfeed-tube--process-info
        :error #'elfeed-tube--handle-retries)
        (when elfeed-tube--debug (message "Fetching info for video '%s'" (elfeed-entry-title entry))))
       (t (message "Could not fetch info for video '%s'" (elfeed-entry-title entry)))))))

(defun elfeed-tube--fetch-alternate (entry &optional db-insert-p attempts)
  (cl-assert (not (null elfeed-tube--invidious-servers)))
  (let ((elfeed-tube-invidious-url
         (elt elfeed-tube--invidious-servers
              (cl-random (length elfeed-tube--invidious-servers)))))
    (elfeed-tube--fetch-maybe entry db-insert-p attempts)))

(elfeed-tube--defcallback elfeed-tube--handle-retries
  (data entry db-insert-p attempts)
  (when elfeed-tube--debug (message "Attempts: %s" attempts))
  (if elfeed-tube--invidious-servers
      (elfeed-tube--fetch-alternate entry db-insert-p (- attempts 1))
    (elfeed-tube--get-invidious-servers
     :success #'elfeed-tube--set-invidious-servers-and-fetch
     :entry entry
     :db-insert-p db-insert-p
     :attempts (- attempts 1))))

(elfeed-tube--defcallback elfeed-tube--set-invidious-servers-and-fetch
  (data entry db-insert-p attempts)
  (setq elfeed-tube--invidious-servers
        (thread-last
          data
          (cl-remove-if-not (lambda (s) (eq t (plist-get (cadr s) :api))))
          (mapcar #'car)))
  (elfeed-tube--fetch-alternate entry db-insert-p (- attempts 1)))

(elfeed-tube--defcallback elfeed-tube--process-info
  (data entry db-insert-p)
  (cl-assert (or (listp data) (vectorp data)))
  (let ((content (elfeed-tube--format-content data))
        (duration (plist-get data :lengthSeconds)))
    (cl-assert (elfeed-entry-p entry))
    (cl-assert (stringp content))
    
    (when elfeed-tube-add-duration
      (setf (elfeed-meta entry :duration) duration))
    
    (if (equal db-insert-p '(4))
        (progn (elfeed-tube--write-db entry content)
               (when (eq elfeed-show-entry entry)
                 (elfeed-show-refresh)))
      (elfeed-tube--hash-content entry content)
      (when (derived-mode-p 'elfeed-show-mode)
        (elfeed-tube-show)))))

(defun elfeed-tube--write-db (entry content)
  (setf (elfeed-entry-content-type entry) 'html)
  (setf (elfeed-entry-content entry) (elfeed-ref content)))

(cl-defun elfeed-tube--hash-content (entry content)
  (cl-assert (stringp content))
  (let ((video-id (elfeed-tube--get-video-id entry)))
    (puthash video-id content elfeed-tube--info-table)))

(defun elfeed-tube--format-content (api-data)
  (let* ((duration-seconds (plist-get api-data :lengthSeconds))
         (duration-minutes (format "%d:%02d"
                                   (floor duration-seconds 60)
                                   (mod   duration-seconds 60)))
         (thumbnail-alist '((large . 2)
                            (medium . 3)
                            (small . 4)))
         (thumbnail-size (cdr-safe (assoc elfeed-tube-thumbnail-size
                                          thumbnail-alist)))
         thumbnail-html)
    (setq thumbnail-html
          (if thumbnail-size
              (concat "<img src=\""
                      (thread-first
                        (plist-get api-data :videoThumbnails)
                        (aref thumbnail-size)
                        (plist-get :url))
                      "\"></a><br><br>")
            ""))
    (concat thumbnail-html
            ;; "<br><p><strong>Duration: " duration-minutes "</strong></p>"
            (replace-regexp-in-string
             "\n" "<br>"
             (plist-get api-data :descriptionHtml)))))

(cl-defun elfeed-tube--get-invidious-servers (&key (attempts nil)
                                                   (success nil)
                                                   (entry nil)
                                                   (db-insert-p nil)
                                                   &allow-other-keys)
  (request "https://api.invidious.io/instances.json"
    :params '(("pretty" . 1)
              ("sort_by" . "type,users"))
    :parser (lambda ()
              (let ((json-object-type (quote plist))
                    (json-array-type (quote list)))
                (json-read)))
    :success (or success #'elfeed-tube--set-invidious-servers)
    :error #'ignore
    :entry entry
    :db-insert-p db-insert-p
    :attempts attempts))

(elfeed-tube--defcallback elfeed-tube--set-invidious-servers (data)
  (setq elfeed-tube--invidious-servers
        (thread-last
          data
          (cl-remove-if-not (lambda (s) (eq t (plist-get (cadr s) :api))))
          (mapcar #'car))))

(defun elfeed-tube-show ()
  "Show fetched video information in an elfeed-show buffer."
  (interactive)
  (when-let* ((_ (and (derived-mode-p 'elfeed-show-mode)
                     (hash-table-p elfeed-tube--info-table)))
             (video-id (elfeed-tube--get-video-id elfeed-show-entry))
             (content (gethash video-id elfeed-tube--info-table)))
    (goto-char (point-max))
    (when (search-backward "(empty)\n" nil t)
      (let* ((inhibit-read-only t)
             (feed (elfeed-entry-feed elfeed-show-entry))
             (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
        (kill-region (point) (point-max))
        (when-let ((_ elfeed-tube-add-duration)
                   (duration-seconds (elfeed-meta elfeed-show-entry :duration)))
          (delete-backward-char 1)
          (beginning-of-line)
          (insert (propertize "Duration: " 'face 'message-header-name))
          (insert (propertize (format "%d:%02d\n"
                                      (floor duration-seconds 60)
                                      (mod   duration-seconds 60))
                              'face 'message-header-other))
          (insert "\n"))
        (elfeed-insert-html content base)))
    (goto-char (point-min))))

(defun elfeed-tube-fetch (entries &optional db-insert-p)
  "Fetch video information for the current Elfeed ENTRIES or entry at point.

With an active region in `elfeed-search-mode', fetch video
information for all selected entries.

The fetched information is cached for this Emacs session only. To
add this information to the Elfeed database, call this command
with prefix argument DB-INSERT-P.

To refetch this data from the Internet, call this command with
double prefix argument DB-INSERT-P."
  (interactive (list (pcase major-mode
                       ('elfeed-search-mode
                        (elfeed-search-selected))
                       ('elfeed-show-mode elfeed-show-entry))
                     current-prefix-arg))
  
  (dolist (entry (or (and (listp entries) entries)
                 (list entries)))
    (if  (elfeed-tube--youtube-p entry)
        (progn (elfeed-tube--fetch-maybe entry db-insert-p)
               (if (cdr-safe entries) (sleep-for 0.2)))
      (message "Not a Youtube video: '%s'" (elfeed-entry-title entry)))))

(defun elfeed-tube-setup (&optional db-insert-p)
  (advice-add elfeed-show-refresh-function :after
              (defun elfeed-tube--fetch-setup ()
                (elfeed-tube-fetch elfeed-show-entry
                                   (when db-insert-p '(4)))))
  (add-hook 'elfeed-new-entry-hook
            (defun elfeed-tube--db-setup (entry)
              (elfeed-tube--fetch-maybe entry
                                        (when db-insert-p '(4))))))

(defun elfeed-tube-teardown ()
  (advice-remove elfeed-show-refresh-function #'elfeed-tube--fetch-setup)
  (remove-hook 'elfeed-new-entry-hook #'elfeed-tube--db-setup))

(provide 'elfeed-tube)
;;; elfeed-tube.el ends here

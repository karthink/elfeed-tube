;;; elfeed-tube.el --- Youtube integration for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: news, hypermedia, convenience

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
(require 'cl-lib)
(require 'subr-x)
(require 'aio)
(require 'elfeed-tube-captions)

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
(defvar elfeed-tube-save-to-db-p nil)
(defvar elfeed-tube--api-video-fields
  '("videoThumbnails" "descriptionHtml" "lengthSeconds"))
(defvar elfeed-tube--max-retries 2)
(defvar elfeed-tube-description-p t)
(defvar elfeed-tube-comments-p nil)
(defvar elfeed-tube-captions-p t)
(defvar elfeed-tube-thumbnail-p t)

;; Helpers
(defsubst elfeed-tube--get-entries ()
  (pcase major-mode
    ('elfeed-search-mode
     (elfeed-search-selected))
    ('elfeed-show-mode
     (list elfeed-show-entry))))

(defsubst elfeed-tube--youtube-p (entry)
  (cl-some (lambda (regex) (string-match-p regex (elfeed-entry-link entry)))
           elfeed-tube-youtube-regexps))

(defsubst elfeed-tube--get-video-id (entry)
  (when (elfeed-tube--youtube-p entry)
    (thread-first (elfeed-entry-id entry)
                  cdr-safe
                  (substring 9))))

(defsubst elfeed-tube--random-elt (collection)
  (and collection 
      (elt collection (cl-random (length collection)))))

(cl-defsubst elfeed-tube--message (str &optional attempts)
  (when elfeed-tube--debug
    (message
     (concat str
             (when (numberp attempts)
               (format " (%d/%d)"
                       (1+ (- elfeed-tube--max-retries
                              attempts))
                       elfeed-tube--max-retries))))
    nil))

(defsubst elfeed-tube--show-thumbnail (thumb)
  (when (and elfeed-tube-thumbnail-p thumb)
    (concat "<img src=\"" thumb "\"></a><br><br>")))

(defsubst elfeed-tube--timestamp (time)
  (format "%d:%02d" (floor time 60) (mod time 60)))

(defsubst elfeed-tube--same-entry-p (entry1 entry2)
  (equal (elfeed-entry-id entry1)
         (elfeed-entry-id entry2)))

(defmacro elfeed-tube--debug (type &rest body)
  (declare (indent defun))
  `(let ((entry (pcase ,type
                  ('show (buffer-local-value
                          'elfeed-show-entry
                          (get-buffer "*elfeed-entry*")))
                  (_ (unless (buffer-live-p
                              (get-buffer "*elfeed-search*"))
                       (save-window-excursion (elfeed-search)))
                     (with-current-buffer (get-buffer "*elfeed-search*")
                       (elfeed-search-selected 'no-region))))))
     ,@body))

;; Data structure
(cl-defstruct
    (elfeed-tube-item (:constructor elfeed-tube-item--create)
                      (:copier nil))
  "Struct to hold elfeed-tube metadata."
  length thumb desc caption error)

(defun elfeed-tube--parse-desc (api-data)
  "test"
  (let* ((length-seconds (plist-get api-data :lengthSeconds))
         (desc-html (replace-regexp-in-string
                     "\n" "<br>"
                     (plist-get api-data :descriptionHtml)))
         (thumb-alist '((large  . 2)
                        (medium . 3)
                        (small  . 4)))
         (thumb-size (cdr-safe (assoc elfeed-tube-thumbnail-size
                                      thumb-alist)))
         (thumb))
    (when (and elfeed-tube-thumbnail-p
               thumb-size)
      (setq thumb (thread-first
                    (plist-get api-data :videoThumbnails)
                    (aref thumb-size)
                    (plist-get :url))))
    `(:length ,length-seconds :thumb ,thumb :desc ,desc-html)))

;; Persistence
(defun elfeed-tube--write-db (entry &optional data-item)
  (cl-assert (elfeed-entry-p entry))
  (when-let* ((data-item (or data-item (elfeed-tube--gethash entry))))
    (when elfeed-tube-description-p 
      (setf (elfeed-entry-content-type entry) 'html)
      (setf (elfeed-meta entry :duration)
            (elfeed-tube-item-length data-item))
      (setf (elfeed-entry-content entry)
            (when-let ((desc (elfeed-tube-item-desc data-item)))
              (elfeed-ref desc))))
    (when elfeed-tube-thumbnail-p 
      (setf (elfeed-meta entry :thumbnail)
            (elfeed-tube-item-thumb data-item)))
    (when elfeed-tube-captions-p
      (setf (elfeed-meta entry :caption)
            (when-let ((caption (elfeed-tube-item-caption data-item))
                       (elfeed-db-directory
                        elfeed-tube-captions-db-directory))
              (elfeed-ref (prin1-to-string caption)))))
    t))

(defun elfeed-tube--gethash (entry)
  (cl-assert (elfeed-entry-p entry))
  (let ((video-id (elfeed-tube--get-video-id entry)))
    (gethash video-id elfeed-tube--info-table)))

(defun elfeed-tube--puthash (entry data-item &optional force)
  (cl-assert (elfeed-entry-p entry))
  (cl-assert (elfeed-tube-item-p data-item))
  (when-let* ((video-id (elfeed-tube--get-video-id entry))
              (_ (or force
                     (not (gethash video-id elfeed-tube--info-table)))))
    ;; (elfeed-tube--message
    ;;  (format "putting %s with data %S" video-id data-item))
    (puthash video-id data-item elfeed-tube--info-table)))

;; Content display
(defun elfeed-tube-show (&optional intended-entry)
  "Show extra video information in an elfeed-show buffer."
  (when-let* ((show-buf (get-buffer "*elfeed-entry*"))
              (entry (buffer-local-value 'elfeed-show-entry show-buf))
              (intended-entry (or intended-entry entry)))
    (when (elfeed-tube--same-entry-p entry intended-entry)
      (with-current-buffer show-buf
        (if-let* ((data-item (elfeed-tube--gethash entry)))
            ;; Load from cache, not db
            (progn
              (let* ((inhibit-read-only t)
                     (feed (elfeed-entry-feed elfeed-show-entry))
                     (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
                (goto-char (point-max))
                (when (text-property-search-backward
                       'face 'message-header-name)
                  (beginning-of-line))
                (elfeed-tube--insert-duration
                 entry (elfeed-tube-item-length data-item))
                (if (or (and (elfeed-tube-item-desc data-item)
                             (not (elfeed-entry-content entry)))
                        (and (elfeed-tube-item-thumb data-item)
                             (not (elfeed-meta entry :thumbnail)))
                        (and (elfeed-tube-item-caption data-item)
                             (not (elfeed-meta entry :caption))))
                    (insert (propertize "[*NOT SAVED*]\n"
                                        'face
                                        '(:inherit message-cited-text-2
                                          :weight bold))))
                
                (when (or (elfeed-tube-item-thumb data-item)
                          (elfeed-tube-item-desc data-item)
                          (elfeed-tube-item-caption data-item))
                  (kill-region (point) (point-max))
                  (open-next-line 1))
                
                (elfeed-insert-html (elfeed-tube--show-desc data-item) base)
                (when elfeed-tube-captions-p
                  (elfeed-tube--insert-captions (elfeed-tube-item-caption data-item)))))
          ;; not in cache, load from db with duration
          (when-let* ((entry elfeed-show-entry)
                      (duration (elfeed-meta elfeed-show-entry :duration))
                      (inhibit-read-only t))
            (elfeed-tube--insert-duration entry duration)
            
            (when-let ((_ elfeed-tube-thumbnail-p)
                       (thumb (elfeed-meta elfeed-show-entry :thumbnail)))
              (goto-char (point-max))
              (text-property-search-backward 'face 'message-header-name)
              (forward-line 2)
              (elfeed-insert-html (elfeed-tube--show-thumbnail thumb)))
            
            (when-let* ((_ elfeed-tube-captions-p)
                        (elfeed-db-directory elfeed-tube-captions-db-directory)
                        (capstr (elfeed-deref
                                 (elfeed-meta elfeed-show-entry :caption)))
                        (caption (read capstr)))
              (elfeed-tube--insert-captions caption))))
        (goto-char (point-min))))))

(defun elfeed-tube--insert-duration (entry duration)
  (if (not (integerp duration))
      (elfeed-tube--message
       (format "Duration not available for video \"%s\""
               (elfeed-entry-title entry)))
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (if (looking-at "Duration:")
          (delete-region (point)
                         (save-excursion (end-of-line)
                                         (point)))
        (open-next-line 1))
      (insert (propertize "Duration: " 'face 'message-header-name)
              (propertize (elfeed-tube--timestamp duration)
                          'face 'message-header-other)
              "\n")
      t)))

(defun elfeed-tube--show-desc (data-item)
  (cl-assert (elfeed-tube-item-p data-item))
  (let ((desc     (elfeed-tube-item-desc data-item))
        (duration (elfeed-tube-item-length data-item))
        (thumb    (elfeed-tube-item-thumb data-item)))
    (concat
     (when elfeed-tube-thumbnail-p
       (elfeed-tube--show-thumbnail thumb))
     (when elfeed-tube-description-p desc))))

;; Setup
(defun elfeed-tube-setup (&optional db-insert-p)
  (defun elfeed-tube--auto-fetch (&optional entry)
    (elfeed-tube--fetch-1 (or entry elfeed-show-entry)
                          (when (or db-insert-p
                                    elfeed-tube-save-to-db-p)
                            '(4))))
  (advice-add elfeed-show-refresh-function :after #'elfeed-tube--auto-fetch)
  (add-hook 'elfeed-new-entry-hook #'elfeed-tube--auto-fetch)
  t)

;; (advice-add elfeed-show-refresh-function :after #'elfeed-tube-show)

(defun elfeed-tube-teardown ()
  (advice-remove elfeed-show-refresh-function #'elfeed-tube--auto-fetch)
  (remove-hook 'elfeed-new-entry-hook #'elfeed-tube--auto-fetch)
  t)

;; From aio-contrib.el: the workhorse
(defun elfeed-tube-curl-enqueue (url &rest args)
  "Like `elfeed-curl-enqueue' but delivered by a promise.

The result is a plist with the following keys:
:success -- the callback argument (t or nil)
:headers -- `elfeed-curl-headers'
:status-code -- `elfeed-curl-status-code'
:error-message -- `elfeed-curl-error-message'
:location -- `elfeed-curl-location'
:content -- (buffer-string)"
  (let* ((promise (aio-promise))
         (cb (lambda (success)
               (let ((result (list :success success
                                   :headers elfeed-curl-headers
                                   :status-code elfeed-curl-status-code
                                   :error-message elfeed-curl-error-message
                                   :location elfeed-curl-location
                                   :content (buffer-string))))
                 (aio-resolve promise (lambda () result))))))
    (prog1 promise
      (apply #'elfeed-curl-enqueue url cb args))))

;; Fetchers
(aio-defun elfeed-tube--get-invidious-servers ()
  (let* ((instances-url (concat "https://api.invidious.io/instances.json"
                                "?pretty=1&sort_by=type,users"))
         (result (aio-await (elfeed-tube-curl-enqueue instances-url :method "GET")))
         (status-code (plist-get result :status-code))
         (servers (plist-get result :content)))
    (when (= status-code 200)
      (thread-last
        (json-parse-string servers :object-type 'plist :array-type 'list)
        (cl-remove-if-not (lambda (s) (eq t (plist-get (cadr s) :api))))
        (mapcar #'car)))))

(aio-defun elfeed-tube--get-invidious-url ()
  (or elfeed-tube-invidious-url
      (let ((servers (or elfeed-tube--invidious-servers
                         (setq elfeed-tube--invidious-servers
                               (aio-await (elfeed-tube--get-invidious-servers))))))
        (elfeed-tube--random-elt servers))))

(aio-defun elfeed-tube--fetch-desc (entry &optional attempts)
  (let* ((attempts (or attempts (1+ elfeed-tube--max-retries)))
         (video-id (elfeed-tube--get-video-id entry)))
    (when (> attempts 0)
      (if-let ((invidious-url (aio-await (elfeed-tube--get-invidious-url))))
          (let* ((api-url (concat
                           invidious-url
                           elfeed-tube--api-videos-path
                           video-id
                           "?fields="
                           (string-join elfeed-tube--api-video-fields ",")))
                 (api-response (aio-await (elfeed-tube-curl-enqueue
                                           api-url
                                           :method "GET")))
                 (api-status (plist-get api-response :status-code))
                 (api-data (plist-get api-response :content))
                 (json-object-type (quote plist)))
            (if (= api-status 200)
                ;; Return data
                (condition-case error
                    (prog1
                        (elfeed-tube--parse-desc
                         (json-parse-string api-data :object-type 'plist)))
                  ('json-parse-error
                   (elfeed-tube--message "Malformed data, retrying fetch"
                                         attempts)
                   (aio-await
                    (elfeed-tube--fetch-desc entry (- attempts 1)))))
              ;; Retry #attempts times
              (elfeed-tube--message
               (format "Fetch failed with code %d, retrying fetch for \"%s\""
                       api-status
                       (elfeed-entry-title entry))
               attempts)
              (aio-await
               (elfeed-tube--fetch-desc entry (- attempts 1)))))

        (message
         "Could not find a valid Invidious url. Please cusomize `elfeed-tube-invidious-url'.")
        nil))))

(aio-defun elfeed-tube--fetch-1 (entry &optional force-fetch)
  (when (elfeed-tube--youtube-p entry)
    (let* ((existing (elfeed-tube--gethash entry))
           (data-item (or existing
                          (elfeed-tube-item--create))))
      
      ;; Record description
      (when (and (or elfeed-tube-thumbnail-p elfeed-tube-description-p)
                 (or force-fetch
                     (not (or existing
                              (elfeed-entry-content entry)))))
        (if-let ((api-data
                  (aio-await (elfeed-tube--fetch-desc entry))))
            (progn
              (when elfeed-tube-thumbnail-p
                (setf (elfeed-tube-item-thumb data-item)
                      (plist-get api-data :thumb)))
              (when elfeed-tube-description-p
                (setf (elfeed-tube-item-length data-item)
                      (plist-get api-data :length))
                (setf (elfeed-tube-item-desc data-item)
                      (plist-get api-data :desc))))
          (push 'desc (elfeed-tube-item-error data-item))))
      
      ;; Record captions
      (when (and elfeed-tube-captions-p
                 (or force-fetch
                     (not (or existing
                              (elfeed-ref-p
                               (elfeed-meta entry :caption))))))
        (if-let ((caption (aio-await (elfeed-tube-captions--fetch entry))))
            (setf (elfeed-tube-item-caption data-item) caption)
          (push 'caption (elfeed-tube-item-error data-item))))
      
      (if elfeed-tube-save-to-db-p
          ;; Store in db
          (progn (elfeed-tube--write-db entry data-item)
                 (when (elfeed-tube--same-entry-p
                        entry elfeed-show-entry)
                   (elfeed-show-refresh))
                 (elfeed-tube--message
                  (format "Saved to elfeed-db: %s"
                          (elfeed-entry-title entry))))
        ;; Store in session cache
        (elfeed-tube--puthash entry data-item force-fetch)
        (elfeed-tube-show entry)))))

;; Entry points
;;;autoload(autoload 'elfeed-tube-fetch "elfeed-tube" "Fetch youtube metadata for Elfeed entries." t nil)
(aio-defun elfeed-tube-fetch (entries &optional force-fetch)
  "Fetch youtube metadata for Elfeed ENTRIES.

In elfeed-show buffers, ENTRIES is the entry being displayed.

In elfeed-search buffers, ENTRIES is the entry at point, or all
entries in the region when the region is active.

With optional prefix argument FORCE-FETCH, force refetching of
the metadata for ENTRIES.

If you want to always add this metadata to the database, consider
setting `elfeed-tube-save-to-db-p'. To customize what kinds of
metadata are fetched, customize TODO
`elfeed-tube-metadata-types'."
  (interactive (list (elfeed-tube--get-entries)
                     current-prefix-arg))
  (if (or elfeed-tube-description-p
          elfeed-tube-captions-p
          elfeed-tube-thumbnail-p)
    (aio-await
     (aio-all
      (cl-loop for entry in (ensure-list entries)
               collect (elfeed-tube--fetch-1 entry force-fetch))))
    (message "Nothing to fetch! Customize `elfeed-tube-metadata-types'.")))

;;;###autoload
(defun elfeed-tube-save (entries)
  "Save elfeed-tube youtube metadata for ENTRIES to the elfeed database.

ENTRIES is the current elfeed entry in elfeed-show buffers. In
elfeed-search buffers it's the entry at point or the selected
entries when the region is active."
  (interactive (list (elfeed-tube--get-entries)))
  (dolist (entry entries)
    (if (elfeed-tube--write-db entry)
        (progn (message "Wrote to elfeed-db: \"%s\"" (elfeed-entry-title entry))
               (when (derived-mode-p 'elfeed-show-mode)
                 (elfeed-show-refresh)))
      (message "elfeed-db already contains: \"%s\"" (elfeed-entry-title entry)))))

(provide 'elfeed-tube)
;;; elfeed-tube.el ends here

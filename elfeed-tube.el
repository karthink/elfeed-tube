(require 'elfeed)
(require 'request)
(require 'cl-lib)

;; ** YOUTUBE METADATA

;; Fetch metadata from Invidious for Youtube feeds
(advice-add elfeed-show-refresh-function :after #'elfeed-tube-show-info)

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

(defcustom elfeed-tube-always-add-to-db nil
  "Should elfeed-tube always add to the Elfeed database?

Setting this to T will always save fetched information to the
Elfeed database.

When set to NIL, you can still add a specific Elfeed entry to the
database by calling `elfeed-tube-add-info' with a prefix
argument. When this option is set to T, the prefix argument
behavior is inverted."
  :group 'elfeed-tube
  :type 'boolean)

(defvar elfeed-tube-invidious-url "https://vid.puffyan.us")
(defvar elfeed-tube-add-duration t)
(defvar elfeed-tube--info-table (make-hash-table :test #'equal))

(defun elfeed-tube--youtube-p (entry)
  (string-match-p "youtube\\.com" (elfeed-entry-link entry)))

(defun elfeed-tube-fetch-info (entry &optional db-insert-p)
  "Fetch video information for the current Elfeed ENTRY or ENTRY at point.

The action of the prefix argument DB-INSERT-P depends on the value of `elfeed-tube-always-add-to-db'.

- When this option is NIL (default), the fetched information is cached for this session only. Caling with prefix argument DB-INSERT-P will cause the fetched information to be written to the Elfeed database instead.
- When this option is T, the fetched information is written to the database by default. Calling with prefix argument DB-INSERT-P will cause the fetched information to be cached for this Emacs session only.

In either case, a double prefix argument will force the fetching of information from the Internet."
  (interactive (list (pcase major-mode
                       ('elfeed-search-mode
                        (elfeed-search-selected 'ignore-region))
                       ('elfeed-show-mode elfeed-show-entry))
                     current-prefix-arg))
  (if  (elfeed-tube--youtube-p entry)
      (elfeed-tube--fetch-info-maybe entry db-insert-p)
    (message "Not at a Youtube video.")))

(defun elfeed-tube--get-video-id (entry)
  (cl-assert (elfeed-entry-p entry))
  (when (elfeed-tube--youtube-p entry)
    (thread-first (elfeed-entry-id entry)
                  cdr-safe
                  (substring 9))))

(defun elfeed-tube--fetch-info-maybe (entry &optional db-insert-p)
  (when-let*
      ((video-id (elfeed-tube--get-video-id entry))
       (api-url (concat
                 elfeed-tube-invidious-url
                 "/api/v1/videos/"
                 video-id)))
    (if-let ((_ (not (equal db-insert-p '(16))))
             (content (gethash video-id elfeed-tube--info-table)))
        (progn
          (message "Info for entry already fetched. Press 'C-u C-u %s' to force refresh." (this-command-keys))
          (elfeed-tube-show-info))
      (request api-url
        :type "GET"
        :params '(("fields" . "videoThumbnails,descriptionHtml,lengthSeconds"))
        :parser (lambda ()
                  (let ((json-object-type (quote plist)))
                    (json-read)))
        :entry entry
        :db-insert-p db-insert-p
        :success #'elfeed-tube--process-info
        :error #'elfeed-tube--handle-error)
      (message "Fetching info for video '%s'" (elfeed-entry-title entry)))))

(cl-defun elfeed-tube--handle-error (&key (data nil)
                                         (error-thrown nil)
                                         (symbol-status nil)
                                         (response nil)
                                         &allow-other-keys)
  (message "request failed with %s" error-thrown))

(cl-defun elfeed-tube--process-info (&key (data nil)
                                         (error-thrown nil)
                                         (symbol-status nil)
                                         (response nil)
                                         &allow-other-keys)
  (cl-assert (or (listp data) (vectorp data)))
  (let* ((content (elfeed-tube--format-content data))
         (duration (plist-get data :lengthSeconds))
         (settings (request-response-settings response))
         (entry (plist-get settings :entry))
         (db-insert-p (plist-get settings :db-insert-p)))
    (cl-assert (elfeed-entry-p entry))
    (cl-assert (stringp content))
    
    (when elfeed-tube-add-duration
      (setf (elfeed-meta entry :duration) duration))
    
    (if (xor (equal db-insert-p '(4)) elfeed-tube-always-add-to-db)
        (progn (setf (elfeed-entry-content-type entry) 'html)
               (setf (elfeed-entry-content entry) (elfeed-ref content))
               (when (eq elfeed-show-entry entry) (elfeed-show-refresh)))
      (elfeed-tube--hash-content entry content)
      (when (derived-mode-p 'elfeed-show-mode)
        (elfeed-tube-show-info)))))

(cl-defun elfeed-tube--hash-content (entry content)
  (cl-assert (elfeed-entry-p entry))
  (cl-assert (stringp content))
  (let ((video-id (elfeed-tube--get-video-id entry)))
    (puthash video-id content elfeed-tube--info-table)))

(defun elfeed-tube-show-info ()
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
              (concat "<a href=\""
                      (elfeed-entry-link entry)
                      "\"><img src=\""
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

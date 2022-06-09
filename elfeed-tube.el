;;; elfeed-tube.el --- Youtube integration for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.10
;; Package-Requires: ((emacs "27.1") (elfeed "3.4.1") (aio "1.0"))
;; Keywords: news, hypermedia, convenience
;; URL: https://github.com/karthink/elfeed-tube

;; This file is NOT part of GNU Emacs.

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
;;
;;; Code:
(require 'elfeed)
(require 'cl-lib)
(require 'subr-x)
(require 'rx)
(require 'aio)

(require 'elfeed-tube-utils)
(require 'elfeed-tube-mpv)

;; Customizatiion options
(defgroup elfeed-tube nil
  "Elfeed-tube: View youtube details in Elfeed"
  :group 'elfeed
  :prefix "elfeed-tube-")

(defcustom elfeed-tube-fields
  '(duration thumbnail description captions)
  "Metadata fields to fetch for youtube entries in Elfeed.

This is a list of symbols. The ordering is not relevant.

The choices are
- duration for video length,
- thumbnail for video thumbnail,
- description for video description,
- captions for video transcript,
- comments for top video comments. (NOT YET IMPLEMENTED)

Other symbols are ignored.

To set the thumbnail size, see `elfeed-tube-thumbnail-size'.
To set caption language(s), see `elfeed-tube-captions-languages'."
  :group 'elfeed-tube
  :type '(repeat (choice (const duration :tag "Duration")
                         (const thumbnail :tag "Thumbnail")
                         (const description :tag "Description")
                         (const captions :tag "Transcript")))) ;TODO

(defcustom elfeed-tube-thumbnail-size 'small
  "Video thumbnail size to show in the Elfeed buffer.

This is a symbol. Choices are large, medium and small. Setting
this to nil to disable showing thumbnails, but customize
`elfeed-tube-fields' for that instead."
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

(defcustom elfeed-tube-captions-languages
  '("english" "english (auto generated)")
  "Caption language priority for elfeed-tube captions.

Captions in the first available langauge in this list will be
fetched. Each entry (string) in the list can be a
language (case-insensitive, \"english\") or language codes:

- \"en\" for English
- \"tr\" for Turkish
- \"ar\" for Arabic, etc

Example:
(\"tr\" \"english\" \"arabic\" \"es\" \"english (auto generated)\")
"
  :group 'elfeed-tube
  :type '(repeat string))

(defcustom elfeed-tube-save-indicator "[*NOT SAVED*]"
  "Indicator to show in Elfeed entry buffers that have unsaved metadata.

This can be set to a string, which will be displayed below the
headers as a button. Activating this button saves the metadata to
the Elfeed database.

If set to any symbol except nil, it displays a minimal indicator
at the top of the buffer instead.

If set to nil, the indicator is disabled."
  :group 'elfeed-tube
  :type '(choice (const  :tag "Disabled" nil)
                 (symbol :tag "Minimal" :value t)
                 (string :tag "Any string")))

(defcustom elfeed-tube-auto-save-p nil
  "Save information fetched by elfeed-tube to the Elfeed databse.

This is a boolean. Fetched information is automatically saved
when this is set to true."
  :group 'elfeed-tube
  :type 'boolean)

(defcustom elfeed-tube-auto-fetch-p t
  "Fetch information automatically when updating Elfeed or opening entries.

This is a boolean. When set to t, video information will be fetched automatically when updating Elfeed or opening video entries that don't have metadata."
  :group 'elfeed-tube
  :type 'boolean)

;; Internal variables
;; (defvar elfeed-tube--debug t)
(defvar elfeed-tube--api-videos-path "/api/v1/videos/")
(defvar elfeed-tube--info-table (make-hash-table :test #'equal))
(defvar elfeed-tube--invidious-servers nil)
(defvar elfeed-tube--sblock-url "https://sponsor.ajay.app")
(defvar elfeed-tube--sblock-api-path "/api/skipSegments")
(defcustom elfeed-tube-captions-sblock-p t
  "Whether sponsored segments should be de-emphasized in transcripts."
  :group 'elfeed-tube
  :type 'boolean)
(defvar elfeed-tube-captions-puntcuate-p t)
(defvar elfeed-tube--api-video-fields
  '("videoThumbnails" "descriptionHtml" "lengthSeconds"))
(defvar elfeed-tube--max-retries 2)
(defvar elfeed-tube--captions-db-dir
  ;; `file-name-concat' is 28.1+ only
  (mapconcat #'file-name-as-directory
             `(,elfeed-db-directory "elfeed-tube" "captions")
             ""))
(defvar elfeed-tube--comments-db-dir
  (mapconcat #'file-name-as-directory
             `(,elfeed-db-directory "elfeed-tube" "comments")
             ""))

(defun elfeed-tube-captions-browse-with (follow-fun)
  (lambda (event)
    (interactive "e")
    (let ((pos (posn-point (event-end event))))
      (funcall follow-fun pos))))

(defvar elfeed-tube-captions-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] (elfeed-tube-captions-browse-with
                               #'elfeed-tube--browse-at-time))
    (define-key map (kbd "RET") #'elfeed-tube-mpv)
    (define-key map [mouse-1] (elfeed-tube-captions-browse-with
                               #'elfeed-tube-mpv))
    map))

(defvar elfeed-tube-caption-faces
  '((text      . variable-pitch)
    (timestamp . (message-header-other :inherit variable-pitch
                                       :weight semi-bold))
    (intro     . (variable-pitch :inherit shadow))
    (outro     . (variable-pitch :inherit shadow))
    (sponsor   . (variable-pitch :inherit shadow
                                 :strike-through t))
    (selfpromo . (variable-pitch :inherit shadow
                                 :strike-through t))))

;; Helpers
(defsubst elfeed-tube-include-p (field)
  (memq field elfeed-tube-fields))

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

(defsubst elfeed-tube-log (level fmt &rest objects)
  (let ((elfeed-log-buffer-name "*elfeed-tube-log*")
        (elfeed-log-level 'debug))
    (apply #'elfeed-log level fmt objects)
    nil))

(defsubst elfeed-tube--attempt-log (attempts)
  (format "(attempt %d/%d)"
          (1+ (- elfeed-tube--max-retries
                 attempts))
          elfeed-tube--max-retries))

(defsubst elfeed-tube--thumbnail-html (thumb)
  (when (and (elfeed-tube-include-p 'thumbnail) thumb)
    (concat "<br><img src=\"" thumb "\"></a><br><br>")))

(defsubst elfeed-tube--timestamp (time)
  (format "%d:%02d" (floor time 60) (mod time 60)))

(defsubst elfeed-tube--same-entry-p (entry1 entry2)
  (equal (elfeed-entry-id entry1)
         (elfeed-entry-id entry2)))

(defsubst elfeed-tube--match-captions-langs (lang el)
  (and (or (string-match-p
            lang
            (thread-first (plist-get el :name)
                          (plist-get :simpleText)))
           (string-match-p
            lang
            (plist-get el :languageCode)))
       el))

(defsubst elfeed-tube--truncate (str)
  (truncate-string-to-width str 20))

(defmacro elfeed-tube--debug (&rest body)
  (declare (indent defun))
  `(let ((entry (if (buffer-live-p (get-buffer "*elfeed-entry*"))
                    (buffer-local-value
                     'elfeed-show-entry
                     (get-buffer "*elfeed-entry*"))
                  (unless (buffer-live-p (get-buffer "*elfeed-search*"))
                    (save-window-excursion (elfeed-search)))
                  (with-current-buffer (get-buffer "*elfeed-search*")
                    (elfeed-search-selected 'no-region)))))
     ,@body))

(defmacro elfeed-tube--with-db (db-dir &rest body)
  "Execute BODY with DB-DIR set as the elfeed-db path."
  (declare (indent defun))
  `(let ((elfeed-db-directory ,db-dir))
     ,@body))

(defsubst elfeed-tube--caption-get-face (type)
  (or (alist-get type elfeed-tube-caption-faces)
      'variable-pitch))

;; Data structure
(cl-defstruct
    (elfeed-tube-item (:constructor elfeed-tube-item--create)
                      (:copier nil))
  "Struct to hold elfeed-tube metadata."
  len thumb desc caps error)

;; Persistence
(defun elfeed-tube--write-db (entry &optional data-item)
  (cl-assert (elfeed-entry-p entry))
  (when-let* ((data-item (or data-item (elfeed-tube--gethash entry))))
    (when (elfeed-tube-include-p 'description)
      (setf (elfeed-entry-content-type entry) 'html)
      (setf (elfeed-entry-content entry)
            (when-let ((desc (elfeed-tube-item-desc data-item)))
              (elfeed-ref desc))))
    (when (elfeed-tube-include-p 'duration)
      (setf (elfeed-meta entry :duration)
            (elfeed-tube-item-len data-item)))
    (when (elfeed-tube-include-p 'thumbnail)
      (setf (elfeed-meta entry :thumb)
            (elfeed-tube-item-thumb data-item)))
    (when (elfeed-tube-include-p 'captions)
      (elfeed-tube--with-db elfeed-tube--captions-db-dir
        (setf (elfeed-meta entry :caps)
              (when-let ((caption (elfeed-tube-item-caps data-item)))
                (elfeed-ref (prin1-to-string caption))))))
    (elfeed-tube-log 'info "[DB][Wrote to DB][video:%s]"
                     (elfeed-tube--truncate (elfeed-entry-title entry)))
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

;; Data munging
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
    (when (and (elfeed-tube-include-p 'thumbnail)
               thumb-size)
      (setq thumb (thread-first
                    (plist-get api-data :videoThumbnails)
                    (aref thumb-size)
                    (plist-get :url))))
    `(:length ,length-seconds :thumb ,thumb :desc ,desc-html)))

(defun elfeed-tube--extract-captions-urls ()
  (catch 'parse-error
    (if (not (search-forward "\"captions\":" nil t))
        (throw 'parse-error "captions section not found")
      (delete-region (point-min) (point))
      (if (not (search-forward ",\"videoDetails" nil t))
          (throw 'parse-error "video details not found")
        (goto-char (match-beginning 0))
        (delete-region (point) (point-max))
        (replace-string-in-region "\n" "" (point-min) (point-max))
        (goto-char (point-min))
        (condition-case error
            (json-parse-buffer :object-type 'plist
                               :array-type 'list)
          ('json-parse-error (throw 'parse-error "json-parse-error")))))))

(defun elfeed-tube--postprocess-captions (text)
  (thread-last
    (string-replace "\n" " " text)
    (replace-regexp-in-string "\\bi\\b" "I")
    (replace-regexp-in-string
     (rx (group (syntax open-parenthesis))
         (one-or-more (or space punct)))
     "\\1")
    (replace-regexp-in-string
     (rx (one-or-more (or space punct))
         (group (syntax close-parenthesis)))
     "\\1")))

;; Content display
(defun elfeed-tube-show (&optional intended-entry)
  "Show extra video information in an elfeed-show buffer."
  (when-let* ((show-buf
               (if intended-entry
                   (get-buffer (elfeed-show--buffer-name intended-entry))
                 (and (elfeed-tube--youtube-p elfeed-show-entry)
                      (current-buffer))))
              (entry (buffer-local-value 'elfeed-show-entry show-buf))
              (intended-entry (or intended-entry entry)))
    (when (elfeed-tube--same-entry-p entry intended-entry)
      (with-current-buffer show-buf
        (let* ((inhibit-read-only t)
               (feed (elfeed-entry-feed elfeed-show-entry))
               (base (and feed (elfeed-compute-base (elfeed-feed-url feed))))
               (data-item (elfeed-tube--gethash entry))
               insertions)
          
          (goto-char (point-max))
          (when (text-property-search-backward
                 'face 'message-header-name)
            (beginning-of-line)
            (when (looking-at "Transcript:")
              (text-property-search-backward
               'face 'message-header-name)
              (beginning-of-line)))
          
          ;; Duration
          (if-let ((_ (elfeed-tube-include-p 'duration))
                   (duration
                    (or (and data-item (elfeed-tube-item-len data-item))
                        (elfeed-meta entry :duration))))
              (elfeed-tube--insert-duration entry duration)
            (forward-line 1))
          
          ;; DB Status
          (when (and
                 elfeed-tube-save-indicator
                 (or (and data-item (elfeed-tube-item-desc data-item)
                          (not (elfeed-entry-content entry)))
                     (and data-item (elfeed-tube-item-thumb data-item)
                          (not (elfeed-meta entry :thumb)))
                     (and data-item (elfeed-tube-item-caps data-item)
                          (not (elfeed-meta entry :caps)))))
            (let ((prop-list
                   `(face (:inherit warning :weight bold) mouse-face highlight
                          help-echo "mouse-1: save this entry to the elfeed-db"
                          keymap ,elfeed-tube--save-state-map)))
              (if (stringp elfeed-tube-save-indicator)
                  (insert (apply #'propertize
                                 elfeed-tube-save-indicator
                                 prop-list)
                          "\n")
                (save-excursion
                    (goto-char (point-min))
                    (end-of-line)
                    (insert " " (apply #'propertize "[∗]" prop-list))))))
          
          ;; Thumbnail
          (when-let ((_ (elfeed-tube-include-p 'thumbnail))
                     (thumb (or (and data-item (elfeed-tube-item-thumb data-item))
                                (elfeed-meta entry :thumb))))
            (elfeed-insert-html (elfeed-tube--thumbnail-html thumb))
            (push 'thumb insertions))
          
          ;; Description
          (delete-region (point) (point-max))
          (when (elfeed-tube-include-p 'description)
            (if-let ((desc (or (and data-item (elfeed-tube-item-desc data-item))
                               (elfeed-deref (elfeed-entry-content entry)))))
                (progn (elfeed-insert-html (concat desc "") base)
                       (push 'desc insertions))))
          
          ;; Captions
          (elfeed-tube--with-db elfeed-tube--captions-db-dir
            (when-let* ((_ (elfeed-tube-include-p 'captions))
                      (caption
                       (or (and data-item (elfeed-tube-item-caps data-item))
                           (and (when-let
                                    ((capstr (elfeed-deref
                                              (elfeed-meta entry :caps))))
                                  (condition-case nil
                                      (read capstr)
                                    ('error
                                     (elfeed-tube-log
                                      'error "[Show][Captions] DB parse error: %S"
                                      (elfeed-meta entry :caps)))))))))
              (when (not (elfeed-entry-content entry))
                (kill-region (point) (point-max)))
              (elfeed-tube--insert-captions caption)
              (push 'caps insertions)))
          
          (if insertions
              (delete-region (point) (point-max))
            (insert (propertize "\n(empty)\n" 'face 'italic))))
        (goto-char (point-min))))))

(defvar elfeed-tube--save-state-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'elfeed-tube-save)
    ;; (define-key map (kbd "RET") #'elfeed-tube--browse-at-time)
    (define-key map [follow-link] 'mouse-face)
    map))

(defun elfeed-tube--insert-duration (entry duration)
  (if (not (integerp duration))
      (elfeed-tube-log
       'warn "[Duration][video:%s][Not available]"
       (elfeed-tube--truncate (elfeed-entry-title entry)))
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (if (looking-at "Duration:")
          (delete-region (point)
                         (save-excursion (end-of-line)
                                         (point)))
	(end-of-line)
	(insert "\n"))
      (insert (propertize "Duration: " 'face 'message-header-name)
              (propertize (elfeed-tube--timestamp duration)
                          'face 'message-header-other)
              "\n")
      t)))

(defun elfeed-tube--insert-captions (caption)
  (if  (and (listp caption)
            (eq (car-safe caption) 'transcript))
      (let ((caption-ordered
             (cl-loop for (type (start dur) text) in (cddr caption)
                      with pstart = 0
                      for oldtime = 0 then time
                      for time = (string-to-number (cdr start))

                      if (< (mod (floor time) 30) (mod (floor oldtime) 30))
                      collect (list pstart time para) into result and
                      do (setq para nil pstart time)
                      
                      collect (cons time
                                    (propertize 
                                     ;; (elfeed-tube--postprocess-captions text)
                                     (string-replace "\n" " " text)
                                     'face (elfeed-tube--caption-get-face type)
                                     'type type))
                      into para
                      finally return (nconc result (list (list pstart time para)))))
            (inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n"
                (propertize "Transcript:" 'face 'message-header-name)
                "\n\n")
        (cl-loop for (start end para) in caption-ordered
                 with beg = (point) do
                 (progn
                   (insert
                    (propertize (format "[%s] - [%s]:\n"
                                        (elfeed-tube--timestamp start)
                                        (elfeed-tube--timestamp end))
                                'face (elfeed-tube--caption-get-face
                                       'timestamp))
                    (propertize "\n" 'hard t)
                    (string-join
                     (mapcar (lambda (tx-cons)
                               (propertize (cdr tx-cons)
                                           'timestamp
                                           (car tx-cons)
                                           'mouse-face
                                           'highlight
                                           'help-echo
                                           #'elfeed-tube--caption-echo
                                           'keymap
                                           elfeed-tube-captions-map))
                             para)
                     " ")
                    (propertize "\n\n" 'hard t)))
                 finally (when-let* ((w shr-width)
                                     (fill-column w)
                                     (use-hard-newlines t))
                           (fill-region beg (point) nil t)))
        ;; (goto-char (point-min))
        )
    (elfeed-tube-log 'debug
                     "[Captions][video:%s][Not available]"
                     (or (and elfeed-show-entry (truncate-string-to-width
                                                 elfeed-show-entry 20))
                         ""))))

(defun elfeed-tube--caption-echo (win obj pos)
  (concat
   (let ((type (get-text-property pos 'type))
         (time (elfeed-tube--timestamp
                (get-text-property pos 'timestamp))))
     (when (not (eq type 'text))
       (format "segment: %s\n\n" (symbol-name type)))
     (format "mouse-1: open video at %s (mpv)\nmouse-2: open video at %s (web browser)" time time))))

;; Setup
(defun elfeed-tube--auto-fetch (&optional entry)
  (when elfeed-tube-auto-fetch-p
    (aio-listen 
     (elfeed-tube--fetch-1 (or entry elfeed-show-entry))
     (lambda (fetched-p)
       (when (funcall fetched-p)
         (elfeed-tube-show (or entry elfeed-show-entry)))))))

(defun elfeed-tube-setup ()
  (add-hook 'elfeed-new-entry-hook #'elfeed-tube--auto-fetch)
  (advice-add 'elfeed-show-entry
              :after #'elfeed-tube--auto-fetch)
  (advice-add elfeed-show-refresh-function
              :after #'elfeed-tube-show)
  t)

(defun elfeed-tube-teardown ()
  (advice-remove elfeed-show-refresh-function #'elfeed-tube-show)
  (advice-remove 'elfeed-show-entry #'elfeed-tube--auto-fetch)
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
      (let ((servers
             (or elfeed-tube--invidious-servers
                 (setq elfeed-tube--invidious-servers
                       (elfeed--shuffle
                        (aio-await (elfeed-tube--get-invidious-servers)))))))
        (car servers))))

(defsubst elfeed-tube--nrotate-invidious-servers ()
  (setq elfeed-tube--invidious-servers
        (nconc (cdr elfeed-tube--invidious-servers)
               (list (car elfeed-tube--invidious-servers)))))

(aio-defun elfeed-tube--fetch-captions-tracks (entry)
  (let* ((video-id (elfeed-tube--get-video-id entry))
         (url (format "https://youtube.com/watch?v=%s" video-id))
         (response (aio-await (elfeed-tube-curl-enqueue url :method "GET")))
         (status-code (plist-get response :status-code)))
    (if-let*
        ((_ (= status-code 200))
         (data (with-temp-buffer
                 (save-excursion (insert (plist-get response :content)))
                 (elfeed-tube--extract-captions-urls))))
      ;; (message "%S" data)
        (thread-first
          data
          (plist-get :playerCaptionsTracklistRenderer)
          (plist-get :captionTracks))
      (elfeed-tube-log 'debug "[%s][Caption tracks]: %s"
                       url (plist-get response :error-message))
      (elfeed-tube-log 'warn "[Captions][video:%s]: Not available"
                       (elfeed-tube--truncate (elfeed-entry-title entry))))))

(aio-defun elfeed-tube--fetch-captions-url (caption-plist entry)
  (let* ((case-fold-search t)
         (chosen-caption
          (cl-loop
           for lang in elfeed-tube-captions-languages
           for pick = (cl-some
                       (lambda (el) (elfeed-tube--match-captions-langs lang el))
                       caption-plist)
           until pick
           finally return pick))
         base-url language)
    (cond
     ((not caption-plist)
      (elfeed-tube-log
       'warn "[Captions][video:%s][No languages]"
       (elfeed-tube--truncate (elfeed-entry-title entry))))
     ((not chosen-caption)
      (elfeed-tube-log
       'warn
       "[Captions][video:%s][Not available in %s]"
       (elfeed-tube--truncate (elfeed-entry-title entry))
       (string-join elfeed-tube-captions-languages ", ")))
     (t (setq base-url (plist-get chosen-caption :baseUrl)
              language (thread-first (plist-get chosen-caption :name)
                                     (plist-get :simpleText)))
        (let* ((response (aio-await (elfeed-tube-curl-enqueue base-url :method "GET")))
               (captions (plist-get response :content))
               (status-code (plist-get response :status-code)))
          (if (= status-code 200)
              (cons language captions)
            (elfeed-tube-log
             'error
             "[Caption][video:%s][lang:%s]: %s"
             (elfeed-tube--truncate (elfeed-entry-title entry))
             language
             (plist-get response :error-message))))))))

(defvar elfeed-tube--sblock-categories
  '("sponsor" "intro" "outro" "selfpromo" "interaction"))

(aio-defun elfeed-tube--fetch-captions-sblock (entry)
  (when-let* ((categories
               (json-serialize (vconcat elfeed-tube--sblock-categories)))
              (api-url (url-encode-url 
                        (concat elfeed-tube--sblock-url
                                elfeed-tube--sblock-api-path
                                "?videoID=" (elfeed-tube--get-video-id entry)
                                "&categories=" categories)))
              (response (aio-await (elfeed-tube-curl-enqueue
                                    api-url :method "GET")))
              (status-code (plist-get response :status-code))
              (content-json (plist-get response :content)))
    (if (= status-code 200)
        (condition-case error
            (json-parse-string content-json
                           :object-type 'plist
                           :array-type 'list)
          ('json-parse-error
           (elfeed-tube-log
            'error
            "[Sponsorblock][video:%s]: JSON malformed"
            (elfeed-tube--truncate (elfeed-entry-title entry)))))
      (elfeed-tube-log
       'error
       "[Sponsorblock][video:%s]: %s"
       (elfeed-tube--truncate (elfeed-entry-title entry))
       (plist-get response :error-message)))))

(aio-defun elfeed-tube--fetch-captions (entry)
  (pcase-let* ((urls (aio-await (elfeed-tube--fetch-captions-tracks entry)))
               (`(,language . ,xmlcaps) (aio-await (elfeed-tube--fetch-captions-url urls entry)))
               (sblock (and elfeed-tube-captions-sblock-p
                            (aio-await (elfeed-tube--fetch-captions-sblock entry))))
               (parsed-caps))
    ;; (print (elfeed-entry-title entry) (get-buffer "*scratch*"))
    ;; (print language (get-buffer "*scratch*"))
    (when xmlcaps 
      (setq parsed-caps (with-temp-buffer
                          (insert xmlcaps)
                          (goto-char (point-min))
                          (dolist (reps '(("&amp;#39;"  . "'")
                                          ("&amp;quot;" . "\"")
                                          ("\n"         . " ")
                                          (" "          . "")))
                            (save-excursion
                              (while (search-forward (car reps) nil t)
                                (replace-match (cdr reps) nil t))))
                          (libxml-parse-xml-region (point-min) (point-max)))))
    (when parsed-caps
      (when (and elfeed-tube-captions-sblock-p sblock)
        (setq parsed-caps (elfeed-tube--sblock-captions sblock parsed-caps))) 
      (when (and elfeed-tube-captions-puntcuate-p
                 (string-match-p "auto-generated" language))
        (elfeed-tube--npreprocess-captions parsed-caps))
      parsed-caps)))

(defun elfeed-tube--npreprocess-captions (captions)
  (cl-loop for text-element in (cddr captions)
           for (type time text) in (cddr captions)
           do (setf (nth 2 text-element)
                    (cl-reduce
                     (lambda (accum reps)
                       (replace-regexp-in-string (car reps) (cdr reps) accum))
                     `(("\\bi\\b" . "I")
                       (,(rx (group (syntax open-parenthesis))
                             (one-or-more (or space punct)))
                        . "\\1")
                       (,(rx (one-or-more (or space punct))
                             (group (syntax close-parenthesis)))
                        . "\\1"))
                     :initial-value text))
           finally return captions))

(defun elfeed-tube--sblock-captions (sblock captions)
  ;; (prin1 (cl-subseq captions 0 5) (get-buffer "*scratch*"))
  (let ((sblock-filtered
         (cl-loop for skip in sblock
                  for cat = (plist-get skip :category)
                  when (member cat elfeed-tube--sblock-categories)
                  collect `(:category ,cat :segment ,(plist-get skip :segment)))))
    (cl-loop for telm in (cddr captions)
             do (when-let
                    ((cat
                      (cl-some
                       (lambda (skip)
                         (pcase-let ((cat (intern (plist-get skip :category)))
                                     (`(,beg ,end) (plist-get skip :segment))
                                     (sn (string-to-number (cdaadr telm))))
                           (and (> sn beg) (< sn end) cat)))
                       sblock-filtered)))
                  (setf (car telm) cat))
             finally return captions)))

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
                 (desc-log (elfeed-tube-log
                            'debug
                            "[Description][video:%s][Fetch:%s]"
                            (elfeed-tube--truncate (elfeed-entry-title entry))
                            api-url))
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
                   (elfeed-tube-log
                    'error
                    "[Description][video:%s]: JSON malformed %s"
                    (elfeed-tube--truncate (elfeed-entry-title entry))
                    (elfeed-tube--attempt-log attempts))
                   (elfeed-tube--nrotate-invidious-servers)
                   (aio-await
                    (elfeed-tube--fetch-desc entry (- attempts 1)))))
              ;; Retry #attempts times
              (elfeed-tube-log 'error
               "[Description][video:%s][%s]: %s %s"
               (elfeed-tube--truncate entry)
               api-url
               (plist-get response :error-message)
               (elfeed-tube--attempt-log attempts))
              (elfeed-tube--nrotate-invidious-servers)
              (aio-await
               (elfeed-tube--fetch-desc entry (- attempts 1)))))

        (message
         "Could not find a valid Invidious url. Please cusomize `elfeed-tube-invidious-url'.")
        nil))))

(aio-defun elfeed-tube--fetch-1 (entry &optional force-fetch)
  (when (elfeed-tube--youtube-p entry)
    (let* ((cached (elfeed-tube--gethash entry))
           desc thumb duration caps error)
      
      ;; When to fetch a field:
      ;; - force-fetch is true: always fetch
      ;; - entry not cached, field not saved: fetch
      ;; - entry not cached but saved: don't fetch
      ;; - entry is cached with errors: don't fetch
      ;; - entry is cached without errors, field not empty: don't fetch
      ;; - entry is saved and field not empty: don't fetch

      ;; Record description
      (when (and (cl-some #'elfeed-tube-include-p
                          '(description duration thumbnail))
                 (or force-fetch
                     (not (or (and cached
                                   (or (cl-intersection
                                        '(desc duration thumb)
                                        (elfeed-tube-item-error cached))
                                       (elfeed-tube-item-len cached)
                                       (elfeed-tube-item-desc cached)
                                       (elfeed-tube-item-thumb cached)))
                              (or (elfeed-entry-content entry)
                                  (elfeed-meta entry :thumb)
                                  (elfeed-meta entry :duration))))))
        (if-let ((api-data
                  (aio-await (elfeed-tube--fetch-desc entry))))
            (progn
              (when (elfeed-tube-include-p 'thumbnail)
                (setf thumb
                      (plist-get api-data :thumb)))
              (when (elfeed-tube-include-p 'description)
                (setf desc
                      (plist-get api-data :desc)))
              (when (elfeed-tube-include-p 'duration)
                (setf duration
                      (plist-get api-data :length))))
          (setq error (append error '(desc duration thumb)))))

      ;; Record captions
      (when (and (elfeed-tube-include-p 'captions)
                 (or force-fetch
                     (not (or (and cached
                                   (or (elfeed-tube-item-caps cached)
                                       (memq 'caps (elfeed-tube-item-error cached))))
                              (elfeed-ref-p
                               (elfeed-meta entry :caps))))))
        (if-let ((caps-new
                  (aio-await (elfeed-tube--fetch-captions entry))))
            (setf caps caps-new)
          (push 'caps error)))

      (if (and elfeed-tube-auto-save-p
               (or duration caps desc thumb))
          ;; Store in db
          (progn (elfeed-tube--write-db
                  entry
                  (elfeed-tube-item--create
                   :len duraion :desc desc :thumb thumb
                   :caps caps))
                 (elfeed-tube-log
                  'info "Saved to elfeed-db: %s"
                  (elfeed-entry-title entry)))
        ;; Store in session cache
        (when (or duration caps desc thumb error)
          (elfeed-tube--puthash
           entry
           (elfeed-tube-item--create
            :len duration :desc desc :thumb thumb
            :caps caps :error error)
           force-fetch)))
      ;; Return t if something was fetched
      (and (or duration caps desc thumb) t))))

;; Interaction
(defun elfeed-tube--browse-at-time (pos)
  (interactive "d")
  (when-let ((time (get-text-property pos 'timestamp)))
    (browse-url (concat "https://youtube.com/watch?v="
                        (elfeed-tube--get-video-id elfeed-show-entry)
                        "&t="
                        (number-to-string (floor time))))))

;; Entry points
(aio-defun elfeed-tube-fetch (entries &optional force-fetch)
  "Fetch youtube metadata for Elfeed ENTRIES.

In elfeed-show buffers, ENTRIES is the entry being displayed.

In elfeed-search buffers, ENTRIES is the entry at point, or all
entries in the region when the region is active.

Outside of Elfeed, prompt the user for any Youtube video URL and
generate an Elfeed-like summary buffer for it.

With optional prefix argument FORCE-FETCH, force refetching of
the metadata for ENTRIES.

If you want to always add this metadata to the database, consider
setting `elfeed-tube-auto-save-p'. To customize what kinds of
metadata are fetched, customize TODO
`elfeed-tube-fields'."
  (interactive (list (or (ensure-list (elfeed-tube--get-entries))
                         (read-from-minibuffer "Youtube video URL: "))
                     current-prefix-arg))
  (if (not (listp entries))
      (elfeed-tube--fake-entry entries force-fetch)
    (if (not elfeed-tube-fields)
        (message "Nothing to fetch! Customize `elfeed-tube-fields'.")
      (dolist (entry (ensure-list entries))
        (aio-await (elfeed-tube--fetch-1 entry force-fetch))
        (elfeed-tube-show entry)))))

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

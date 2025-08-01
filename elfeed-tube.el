;;; elfeed-tube.el --- YouTube integration for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.15
;; Package-Requires: ((emacs "27.1") (elfeed "3.4.1") (aio "1.0"))
;; Keywords: news, hypermedia, convenience
;; URL: https://github.com/karthink/elfeed-tube

;; SPDX-License-Identifier: UNLICENSE

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Elfeed Tube is an extension for Elfeed, the feed reader for Emacs, that
;; enhances your Youtube RSS feed subscriptions.
;;
;; Typically Youtube RSS feeds contain only the title and author of each video.
;; Elfeed Tube adds video descriptions, thumbnails, durations, chapters and
;; "live" transcrips to video entries. See
;; https://github.com/karthink/elfeed-tube for demos. This information can
;; optionally be added to your entry in your Elfeed database.
;;
;; The displayed transcripts and chapter headings are time-aware, so you can
;; click on any transcript segment to visit the video at that time (in a browser
;; or your video player if you also have youtube-dl). A companion package,
;; `elfeed-tube-mpv', provides complete mpv (video player) integration with the
;; transcript, including video seeking through the transcript and following
;; along with the video in Emacs.
;;
;; To use this package,
;;
;; (i) Subscribe to Youtube channel or playlist feeds in Elfeed. You can use the
;; helper function `elfeed-tube-add-feeds' provided by this package to search for
;; Youtube channels by URLs or search queries.
;;
;; (ii) Place in your init file the following:
;;
;; (require 'elfeed-tube)
;; (elfeed-tube-setup)
;;
;; (iii) Use Elfeed as normal, typically with `elfeed'. Your Youtube feed
;; entries should be fully populated.
;;
;; You can also call `elfeed-tube-fetch' in an Elfeed buffer to manually
;; populate an entry, or obtain an Elfeed entry-like summary for ANY youtube
;; video (no subscription needed) by manually calling `elfeed-tube-fetch' from
;; outside Elfeed.
;; 
;; User options:
;;
;; There are three options of note:
;;
;; `elfeed-tube-fields': Customize this to set the kinds of metadata you want
;; added to Elfeed's Youtube entries. You can selectively turn on/off
;; thumbnails, transcripts etc.
;;
;; `elfeed-tube-auto-save-p': Set this boolean to save fetched Youtube metadata
;; to your Elfeed database, i.e. to persist the data on disk for all entries.
;;
;; `elfeed-tube-auto-fetch-p': Unset this boolean to turn off fetching metadata.
;; You can then call `elfeed-tube-fetch' to manually fetch data for specific
;; feed entries.
;;
;; See the customization group `elfeed-tube' for more options. See the README
;; for more information.
;; 
;;; Code:
(require 'elfeed)
(eval-when-compile
  (require 'cl-lib))
(require 'subr-x)
(require 'rx)
(require 'map)
(require 'aio)

(require 'elfeed-tube-utils)

;; Customizatiion options
(defgroup elfeed-tube nil
  "Elfeed-tube: View youtube details in Elfeed."
  :group 'elfeed
  :prefix "elfeed-tube-")

(defcustom elfeed-tube-fields
  '(duration thumbnail description captions chapters)
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
  :type '(repeat (choice (const :tag "Duration" duration)
                         (const :tag "Thumbnail" thumbnail)
                         (const :tag "Description" description)
                         (const :tag "Transcript" captions)))) ;TODO

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

(defcustom elfeed-tube-youtube-regexp
  (rx bol
      (zero-or-one (or "http://" "https://"))
      (zero-or-one "www.")
      (or "youtube.com/" "youtu.be/"))
  "Regular expression to match Elfeed entry URLss against.

Only entries that match this regexp will be handled by
elfeed-tube when fetching information."
  :group 'elfeed-tube
  :type 'string)

(defcustom elfeed-tube-captions-languages
  '("en" "english" "english (auto generated)")
  "Caption language priority for elfeed-tube captions.

Captions in the first available langauge in this list will be
fetched. Each entry (string) in the list can be a language code
or a language name (case-insensitive, \"english\"):

- \"en\" for English
- \"tr\" for Turkish
- \"ar\" for Arabic
- \"de\" for German
- \"pt-BR\" for Portugese (Brazil), etc

Example:
 (\"tr\" \"es\" \"arabic\" \"english\" \"english (auto generated)\")

NOTE: Language codes are safer to use. Language full names differ
across regions. For example, \"english\" would be spelled
\"englisch\" if you are in Germany."
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
  "Save information fetched by elfeed-tube to the Elfeed database.

This is a boolean. Fetched information is automatically saved
when this is set to true."
  :group 'elfeed-tube
  :type 'boolean)

(defcustom elfeed-tube-auto-fetch-p t
  "Fetch infor automatically when updating Elfeed or opening entries.

This is a boolean. When set to t, video information will be
fetched automatically when updating Elfeed or opening video
entries that don't have metadata."
  :group 'elfeed-tube
  :type 'boolean)

(defcustom elfeed-tube-backend 'youtube
  "Backend for fetching video metadata from YouTube.

This is a symbol, with recognized values

- youtube: Fetch data from YouTube directly (the default),
- yt-dlp: Fetch data using yt-dlp (which must be on the path)
- invidious: Fetch data using Invidious (currently non-operational)."
  :group 'elfeed-tube
  :type '(choice
          (const :tag "YouTube" youtube)
          (const :tag "yt-dlp"  yt-dlp)
          (const :tag "Invidious" invidious)))

(defvar elfeed-tube-use-ytdlp-p (and (executable-find "yt-dlp") t)
  "Whether to use yt-dlp to obtain description, thumbnail, duration.

This is a boolean. Use yt-dlp when t. Otherwise use invidious.")
(make-obsolete-variable
 'elfeed-tube-use-ytdlp-p 'elfeed-tube-backend "0.16")

(defcustom elfeed-tube-captions-sblock-p t
  "Whether sponsored segments should be de-emphasized in transcripts."
  :group 'elfeed-tube
  :type 'boolean)

(defcustom elfeed-tube-captions-chunk-time 30
  "Chunk size used when displaying video transcripts.

This is the number of seconds of the transcript to chunk into
paragraphs or sections. It must be a positive integer."
  :group 'elfeed-tube
  :type 'integer)

;;; Internal variables
(defvar elfeed-tube--info-table (make-hash-table :test #'equal))
(defvar elfeed-tube--sblock-url "https://sponsor.ajay.app")
(defvar elfeed-tube--sblock-api-path "/api/skipSegments")
(defvar elfeed-tube-captions-puntcuate-p t)

(defvar elfeed-tube--captions-db-dir
  ;; `file-name-concat' is 28.1+ only
  (mapconcat #'file-name-as-directory
             `(,elfeed-db-directory "elfeed-tube" "captions")
             ""))
(defvar elfeed-tube--comments-db-dir
  (mapconcat #'file-name-as-directory
             `(,elfeed-db-directory "elfeed-tube" "comments")
             ""))

(defconst elfeed-tube--innertube-context
  '((client
     (clientName . "ANDROID")
     (clientVersion . "20.10.38"))))

(defconst elfeed-tube--innertube-api-url
  "https://www.youtube.com/youtubei/v1/player")

(defun elfeed-tube-captions-browse-with (follow-fun)
  "Return a command to browse thing at point with FOLLOW-FUN."
  (lambda (event)
    "Translate mouse event to point based button action."
    (interactive "e")
    (let ((pos (posn-point (event-end event))))
      (funcall follow-fun pos))))

(defvar elfeed-tube-captions-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] (elfeed-tube-captions-browse-with
                               #'elfeed-tube--browse-at-time))
    map))

(defface elfeed-tube-chapter-face
  '((t :inherit (variable-pitch message-header-other) :weight bold))
  "Face used for chapter headings displayed by Elfeed Tube.")

(defface elfeed-tube-timestamp-face
  '((t :inherit (variable-pitch message-header-other) :weight semi-bold))
  "Face used for transcript timestamps displayed by Elfeed Tube.")

(defvar elfeed-tube-captions-faces
  '((text      . variable-pitch)
    (timestamp . elfeed-tube-timestamp-face)
    (intro     . (variable-pitch :inherit shadow))
    (outro     . (variable-pitch :inherit shadow))
    (sponsor   . (variable-pitch :inherit shadow
                                 :strike-through t))
    (selfpromo . (variable-pitch :inherit shadow
                                 :strike-through t))
    (chapter   . elfeed-tube-chapter-face)))

;;; Data structure
(cl-defstruct
    (elfeed-tube-item (:constructor elfeed-tube-item--create)
                      (:copier nil))
  "Struct to hold elfeed-tube metadata."
  len thumb desc caps error)

;;; Persistence
(defun elfeed-tube--write-db (entry &optional data-item)
  "Write struct DATA-ITEM to Elfeed ENTRY in `elfeed-db'."
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
  "Get hashed elfeed-tube data for ENTRY."
  (cl-assert (elfeed-entry-p entry))
  (let ((video-id (elfeed-tube--entry-video-id entry)))
    (gethash video-id elfeed-tube--info-table)))

(defun elfeed-tube--puthash (entry data-item &optional force)
  "Cache elfeed-dube-item DATA-ITEM for ENTRY."
  (cl-assert (elfeed-entry-p entry))
  (cl-assert (elfeed-tube-item-p data-item))
  (when-let* ((video-id (elfeed-tube--entry-video-id entry))
              (f (or force
                     (not (gethash video-id elfeed-tube--info-table)))))
    ;; (elfeed-tube--message
    ;;  (format "putting %s with data %S" video-id data-item))
    (puthash video-id data-item elfeed-tube--info-table)))

;;; Content display
(defvar elfeed-tube--save-state-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'elfeed-tube-save)
    ;; (define-key map (kbd "RET") #'elfeed-tube--browse-at-time)
    (define-key map [follow-link] 'mouse-face)
    map))

(defun elfeed-tube-show (&optional intended-entry)
  "Show extra video information in an Elfeed entry buffer.

INTENDED-ENTRY is the Elfeed entry being shown. If it is not
specified use the entry (if any) being displayed in the current
buffer."
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
          (if-let ((d (elfeed-tube-include-p 'duration))
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
          (when-let ((th (elfeed-tube-include-p 'thumbnail))
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
            (when-let* ((c (elfeed-tube-include-p 'captions))
                      (caption
                       (or (and data-item (elfeed-tube-item-caps data-item))
                           (and (when-let
                                    ((capstr (elfeed-deref
                                              (elfeed-meta entry :caps))))
                                  (condition-case nil
                                      (read capstr)
                                    (error
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

        (setq-local
         imenu-prev-index-position-function #'elfeed-tube-prev-heading
         imenu-extract-index-name-function #'elfeed-tube--line-at-point)
        
        (goto-char (point-min))))))

(defun elfeed-tube--insert-duration (entry duration)
  "Insert the video DURATION for ENTRY into an Elfeed entry buffer."
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
  "Insert the video CAPTION for ENTRY into an Elfeed entry buffer."
  (if  (and (listp caption)
            (eq (car-safe caption) 'transcript))
      (let ((caption-ordered
             (cl-loop for (type (start _) text) in (cddr caption)
                      with chapters = (car-safe (cdr caption))
                      with pstart = 0
                      for chapter = (car-safe chapters)
                      for oldtime = 0 then time
                      for time = (string-to-number (cdr start))
                      for chap-begin-p =
                      (and chapter
                           (>= (floor time) (string-to-number (car chapter))))

                      if (and
                          (or chap-begin-p
                              (< (mod (floor time)
                                      elfeed-tube-captions-chunk-time)
                                 (mod (floor oldtime)
                                      elfeed-tube-captions-chunk-time)))
                          (> (abs (- time pstart)) 3))
                      collect (list pstart time para) into result and
                      do (setq para nil pstart time)
                      
                      if chap-begin-p
                      do (setq chapters (cdr-safe chapters))
                      
                      when text
                      collect (cons time
                                    (propertize
                                     ;; (elfeed-tube--postprocess-captions text)
                                     (replace-regexp-in-string "\n" " " text)
                                     'face (elfeed-tube--caption-get-face type)
                                     'type type))
                      into para
                      finally return (nconc result (list (list pstart time para)))))
            (inhibit-read-only t))
        (goto-char (point-max))
        (unless (eq (char-before) 10) (insert "\n"))
        (insert "\n"
                (propertize "Transcript:" 'face 'message-header-name)
                "\n\n")
        (cl-loop for (start end para) in caption-ordered
                 with chapters = (car-safe (cdr caption))
                 with vspace = (propertize " " 'face 'variable-pitch)
                 for chapter = (car-safe chapters)
                 with beg = (point) do
                 (progn
                   (when (and chapter (>= start (string-to-number (car chapter))))
                     (insert (propertize (cdr chapter)
                                         'face
                                         (elfeed-tube--caption-get-face 'chapter)
                                         'timestamp (string-to-number (car chapter))
                                         'mouse-face 'highlight
                                         'help-echo #'elfeed-tube--caption-echo
                                         'keymap elfeed-tube-captions-map
                                         'type 'chapter)
                             (propertize "\n\n" 'hard t))
                     (setq chapters (cdr chapters)))
                   (insert
                    (propertize (format "[%s] - [%s]:"
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
                     vspace)
                    (propertize "\n\n" 'hard t)))
                 finally (when-let* ((w shr-width)
                                     (fill-column w)
                                     (use-hard-newlines t))
                           (fill-region beg (point) nil t))))
    (elfeed-tube-log 'debug
                     "[Captions][video:%s][Not available]"
                     (or (and elfeed-show-entry (truncate-string-to-width
                                                 elfeed-show-entry 20))
                         ""))))

(defvar elfeed-tube--captions-echo-message
  (lambda (time) (format "mouse-2: open at %s (web browser)" time)))

(defun elfeed-tube--caption-echo (_ _ pos)
  "Caption echo text at position POS."
  (concat
   (when-let ((type (get-text-property pos 'type)))
     (when (not (eq type 'text))
       (format "  segment: %s\n\n" (symbol-name type))))
   (let ((time (elfeed-tube--timestamp
                (get-text-property pos 'timestamp))))
     (funcall elfeed-tube--captions-echo-message time))))

;;; Setup
(defun elfeed-tube--auto-fetch (&optional entry)
  "Fetch video information for Elfeed ENTRY and display it if possible.

If ENTRY is not specified, use the entry (if any) corresponding
to the current buffer."
  (when elfeed-tube-auto-fetch-p
    (aio-listen
     (elfeed-tube--fetch-1 (or entry elfeed-show-entry))
     (lambda (fetched-p)
       (when (funcall fetched-p)
         (elfeed-tube-show (or entry elfeed-show-entry)))))))

(defun elfeed-tube-setup ()
  "Set up elfeed-tube.

This does the following:
- Enable fetching video metadata when running `elfeed-update'.
- Enable showing video metadata in `elfeed-show' buffers if available."
  (add-hook 'elfeed-new-entry-hook #'elfeed-tube--auto-fetch)
  (advice-add 'elfeed-show-entry
              :after #'elfeed-tube--auto-fetch)
  (advice-add elfeed-show-refresh-function
              :after #'elfeed-tube-show)
  t)

(defun elfeed-tube-teardown ()
  "Undo the effects of `elfeed-tube-setup'."
  (advice-remove elfeed-show-refresh-function #'elfeed-tube-show)
  (advice-remove 'elfeed-show-entry #'elfeed-tube--auto-fetch)
  (remove-hook 'elfeed-new-entry-hook #'elfeed-tube--auto-fetch)
  t)

;;; Data munging
;;;; Captions
(defun elfeed-tube--get-chapters (desc)
  "Get chapter timestamps from video DESC."
  (with-temp-buffer
    (let ((chapters))
      (save-excursion (insert desc))
      (while (re-search-forward
              ;; "<a href=.*?data-jump-time=\"\\([0-9]+\\)\".*?</a>\\(?:\\s-\\|\\s)\\|-\\)+\\(.*\\)$"
              "\\(?:\\([0-9]+\\):\\)?\\([0-9]+?\\):\\([0-9]\\{2\\}\\) *-? *\\(.*\\)$"
              nil t)
        (push (cons (number-to-string   ;MM:SS timestamp
                     (+ (if (match-string 1)
                            (* 3600 (string-to-number (match-string 1)))
                          0)
                        (* 60 (string-to-number (match-string 2)))
                        (string-to-number (match-string 3))))
                    (match-string 4)
                    ;; (match-string 1)
                    ;; (thread-last (match-string 2)
                    ;;              (replace-regexp-in-string
                    ;;               "&quot;" "\"")
                    ;;              (replace-regexp-in-string
                    ;;               "&#39;" "'")
                    ;;              (replace-regexp-in-string
                    ;;               "&amp;" "&")
                    ;;              (string-trim))
                    )
              chapters))
      (nreverse chapters))))

(defun elfeed-tube--postprocess-captions (text)
  "Tweak TEXT for display in the transcript."
  (thread-last
    ;; (string-replace "\n" " " text)
    (replace-regexp-in-string "\n" " " text)
    (replace-regexp-in-string "\\bi\\b" "I")
    (replace-regexp-in-string
     (rx (group (syntax open-parenthesis))
         (one-or-more (or space punct)))
     "\\1")
    (replace-regexp-in-string
     (rx (one-or-more (or space punct))
         (group (syntax close-parenthesis)))
     "\\1")))

;;;; Sponsorblock
(defvar elfeed-tube--sblock-categories
  '("sponsor" "intro" "outro" "selfpromo" "interaction"))

(aio-defun elfeed-tube--fetch-captions-sblock (entry)
  (when-let* ((categories
               (json-serialize (vconcat elfeed-tube--sblock-categories)))
              (api-url (url-encode-url
                        (concat elfeed-tube--sblock-url
                                elfeed-tube--sblock-api-path
                                "?videoID=" (elfeed-tube--entry-video-id entry)
                                "&categories=" categories)))
              (response (aio-await (elfeed-tube-curl-enqueue
                                    api-url :method "GET")))
              (status-code (plist-get response :status-code))
              (content-json (plist-get response :content)))
    (if (equal status-code 200)
        (condition-case nil
            (json-parse-string content-json
                           :object-type 'plist
                           :array-type 'list)
          (json-parse-error
           (elfeed-tube-log
            'error
            "[Sponsorblock][video:%s]: JSON malformed"
            (elfeed-tube--truncate (elfeed-entry-title entry)))))
      (elfeed-tube-log
       'error
       "[Sponsorblock][video:%s]: %s"
       (elfeed-tube--truncate (elfeed-entry-title entry))
       (plist-get response :error-message)))))

(defun elfeed-tube--npreprocess-captions (captions)
  "Preprocess CAPTIONS."
  (cl-loop for text-element in (cddr captions)
           for (_ _ text) in (cddr captions)
           with reps-list = `(("\\bi\\b" . "I")
                              (,(rx (group (syntax open-parenthesis))
                                 (one-or-more (or space punct)))
                               . "\\1")
                              (,(rx (one-or-more (or space punct))
                                 (group (syntax close-parenthesis)))
                               . "\\1"))
           when (cddr text-element)
           do (setf (nth 2 text-element)
                    (cl-reduce
                     (lambda (accum reps)
                       (replace-regexp-in-string (car reps) (cdr reps) accum))
                     reps-list
                     :initial-value text))
           finally return captions))

(defun elfeed-tube--sblock-captions (sblock captions)
  "Add sponsor data from SBLOCK into CAPTIONS."
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

;;; Youtube backend
(defun elfeed-tube--youtube-parse-desc (api-data)
  "Parse API-DATA for video description and captions."
  (when-let* ((details (plist-get api-data :videoDetails)))
    (let* ((desc (plist-get details :shortDescription))
           (chapters (elfeed-tube--get-chapters desc))
           (length-seconds (plist-get details :lengthSeconds))
           (thumb-alist '((large  . 3) (medium . 2) (small  . 1)))
           (thumb-idx (cdr-safe (assoc elfeed-tube-thumbnail-size
                                       thumb-alist)))
           thumb)
      (when (and (elfeed-tube-include-p 'thumbnail) thumb-idx)
        (setq thumb (map-nested-elt
                     details `(:thumbnail :thumbnails ,thumb-idx :url)))
        (list :length (if (stringp length-seconds)
                          (string-to-number length-seconds)
                        length-seconds)
              :thumb thumb
              :desc (replace-regexp-in-string "\n" "<br>" desc)
              :chaps chapters)))))

(aio-defun elfeed-tube--youtube-fetch-captions-url (caption-plist entry)
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
     (t (setq base-url (replace-regexp-in-string
                        "&fmt=srv3" "" (plist-get chosen-caption :baseUrl))
              language (or (map-nested-elt chosen-caption '(:name :runs 0 :text))
                           ;; Legacy: may not work any more
                           (thread-first (plist-get chosen-caption :name)
                                         (plist-get :simpleText))))
        (let* ((response (aio-await (elfeed-tube-curl-enqueue base-url :method "GET")))
               (captions (plist-get response :content))
               (status-code (plist-get response :status-code)))
          (if (equal status-code 200)
              (cons language captions)
            (elfeed-tube-log
             'error
             "[Caption][video:%s][lang:%s]: %s"
             (elfeed-tube--truncate (elfeed-entry-title entry))
             language
             (plist-get response :error-message))))))))

(aio-defun elfeed-tube--youtube-fetch-innertube (video-id)
  (let* ((url (format "https://youtube.com/watch?v=%s" video-id))
         (response (aio-await (elfeed-tube-curl-enqueue url :method "GET")))
         (status-code (plist-get response :status-code)))
    (if-let* (((equal status-code 200))
              (video-html (plist-get response :content))
              (innertube-api-key
               (and (string-match
                     "\"INNERTUBE_API_KEY\":\"\\([a-zA-Z0-9_-]+\\)" video-html)
                    (match-string 1 video-html)))
              (innertube-response
               (aio-await
                (elfeed-tube-curl-enqueue
                 (format "%s?key=%s"
                         elfeed-tube--innertube-api-url
                         innertube-api-key)
                 :method "POST"
                 :headers '(("Content-Type" . "application/json")
                            ("Accept-Language" . "en-US"))
                 :data (json-encode
                        `((context . ,elfeed-tube--innertube-context)
                          (videoId . ,video-id))))))
              ((= (plist-get innertube-response :status-code) 200)))
        (condition-case nil
            (json-parse-string (plist-get innertube-response :content)
                               :object-type 'plist)
          (json-parse-error
           (prog1 nil
             (elfeed-tube-log
              'error "[Description][video:%s]: JSON malformed" video-id))))
      (prog1 nil
        (elfeed-tube-log 'debug "[%s][Description]: %s"
                         url (plist-get response :error-message))
        (elfeed-tube-log 'warn "[Description][video:%s]: Not available" video-id)))))

(aio-defun elfeed-tube--youtube-fetch-captions (urls entry)
  (pcase-let* ((`(,language . ,xmlcaps)
                (aio-await (elfeed-tube--youtube-fetch-captions-url urls entry)))
               (sblock (and elfeed-tube-captions-sblock-p
                            (aio-await (elfeed-tube--fetch-captions-sblock entry))))
               (parsed-caps))
    (when xmlcaps
      (setq parsed-caps (with-temp-buffer
                          (insert xmlcaps)
                          (goto-char (point-min))
                          (dolist (reps '(("\n"    . " ")
                                          ("&amp;" . "&")
                                          (" "     . "")))
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

(aio-defun elfeed-tube--youtube-fetch-desc (entry &optional existing attempts)
  (let ((attempts (or attempts (1+ elfeed-tube--max-retries)))
        (video-id (elfeed-tube--entry-video-id entry)))
    (when (> attempts 0)
      (let ((caps))
        (if-let* ((api-data
                   (or existing
                       (aio-await (elfeed-tube--youtube-fetch-innertube video-id)))))
            (progn
              ;; Captions
              (when (elfeed-tube-include-p 'captions)
                (if-let* ((caption-tracks
                           (map-nested-elt api-data '(:captions
                                                      :playerCaptionsTracklistRenderer
                                                      :captionTracks))))
                    (setq caps (aio-await (elfeed-tube--youtube-fetch-captions
                                           caption-tracks entry)))
                  (elfeed-tube-log 'warn "[Captions][video:%s]: Not available"
                                   (elfeed-tube--truncate (elfeed-entry-title entry)))))
              ;; Rest
              (nconc (elfeed-tube--youtube-parse-desc api-data)
                     (and caps (list :caps caps))))
          (aio-await (elfeed-tube--youtube-fetch-desc entry nil (- attempts 1))))))))

;;; Fetching
(declare-function elfeed-tube--ytdlp-fetch-desc "elfeed-tube-ytdlp")
(declare-function elfeed-tube--invidious-fetch-desc "elfeed-tube-invidious")

(defun elfeed-tube--fetch-desc (entry &optional existing attempts)
  "Fetch metadata for ENTRY, try ATTEMPTS times."
  (when (eq elfeed-tube-backend 'yt-dlp) (require 'elfeed-tube-ytdlp))
  (pcase-exhaustive elfeed-tube-backend
    ('youtube (elfeed-tube--youtube-fetch-desc entry existing attempts))
    ('yt-dlp
     (require 'elfeed-tube-ytdlp)
     (elfeed-tube--ytdlp-fetch-desc entry existing attempts))
    ('invidious
     (require 'elfeed-tube-invidious)
     (elfeed-tube--invidious-fetch-desc entry existing attempts))))

(aio-defun elfeed-tube--with-label (label func &rest args)
  (cons label (aio-await (apply func args))))

(aio-defun elfeed-tube--fetch-1 (entry &optional force-fetch existing)
  (when (elfeed-tube--youtube-p entry)
   (let* ((cached (elfeed-tube--gethash entry))
          desc thumb duration caps sblock chaps error)
      
    ;; When to fetch a field:
    ;; - force-fetch is true: always fetch
    ;; - entry not cached, field not saved: fetch
    ;; - entry not cached but saved: don't fetch
    ;; - entry is cached with errors: don't fetch
    ;; - entry is cached without errors, field not empty: don't fetch
    ;; - entry is saved and field not empty: don't fetch

    (when (or force-fetch
           (not (or (and cached
                     (or (cl-intersection
                          '(desc duration thumb caps)
                          (elfeed-tube-item-error cached))
                      (elfeed-tube-item-len cached)
                      (elfeed-tube-item-desc cached)
                      (elfeed-tube-item-thumb cached)))
                 (or (elfeed-entry-content entry)
                  (elfeed-meta entry :thumb)
                  (elfeed-meta entry :duration)
                  (elfeed-meta entry :caps)))))
      
      (if-let* ((data (aio-await (elfeed-tube--fetch-desc entry existing))))
          (progn
            (when (elfeed-tube-include-p 'thumbnail)
              (setf thumb (plist-get data :thumb)))
            (when (elfeed-tube-include-p 'description)
              (setf desc (plist-get data :desc)))
            (when (elfeed-tube-include-p 'duration)
              (setf duration (plist-get data :length)))
            (when (elfeed-tube-include-p 'chapters)
              (setf chaps (plist-get data :chaps)))
            (when (elfeed-tube-include-p 'captions)
              (setf caps (plist-get data :caps))))
        (setq error (append error '(desc duration thumb caps)))))
      
    ;; Add (optional) sblock and chapter info to caps
    (when caps
     (when chaps (setf (cadr caps) chaps)))
      
    (if (and elfeed-tube-auto-save-p
         (or duration caps desc thumb))
        ;; Store in db
        (progn (elfeed-tube--write-db
                entry
                (elfeed-tube-item--create
                 :len duration :desc desc :thumb thumb
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

;;; Entry points

;;;###autoload (autoload 'elfeed-tube-fetch "elfeed-tube" "Fetch youtube metadata for Youtube video or Elfeed entry ENTRIES." t nil)
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
  (interactive (list (or (elfeed-tube--ensure-list (elfeed-tube--get-entries))
                         (read-from-minibuffer "Youtube video URL: "))
                     current-prefix-arg))
  (if (not (listp entries))
      (elfeed-tube--fake-entry entries force-fetch)
    (if (not elfeed-tube-fields)
        (message "Nothing to fetch! Customize `elfeed-tube-fields'.")
      (dolist (entry (elfeed-tube--ensure-list entries))
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

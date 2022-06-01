;; -*- lexical-binding: t; -*-

(defcustom elfeed-tube-captions-languages
  '("english" "english (auto generated)")
  "Caption language priority for elfeed-tube captions.

Captions in the first available langauge in this list will be fetched. Each entry (string) in the list can be a language (case-insensitive, \"english\") or language codes:
- \"en\" for English
- \"tr\" for Turkish
- \"ar\" for Arabic, etc

Example: (\"tr\" \"english\" \"arabic\" \"es\")
"
  :group 'elfeed-tube
  :type '(repeat string))

(defvar elfeed-tube-captions-db-directory
  (concat (file-name-as-directory
           elfeed-db-directory)
          "captions"))

(defsubst elfeed-tube-captions--match-lang (lang el)
  (and (or (string-match-p
            lang
            (thread-first (plist-get el :name)
                          (plist-get :simpleText)))
           (string-match-p
            lang
            (plist-get el :languageCode)))
       el))

(defun elfeed-tube-captions--extract-urls ()
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

(aio-defun elfeed-tube-captions--fetch-tracks (entry)
  (let* ((video-id (elfeed-tube--get-video-id entry))
         (url (format "https://youtube.com/watch?v=%s" video-id))
         (response (aio-await (elfeed-tube-curl-enqueue url :method "GET")))
         (status-code (plist-get response :status-code)))
    (when-let*
        ((_ (= status-code 200))
         (data (with-temp-buffer
                 (save-excursion (insert (plist-get response :content)))
                 (elfeed-tube-captions--extract-urls))))
      ;; (message "%S" data)
      (thread-first
        data
        (plist-get :playerCaptionsTracklistRenderer)
        (plist-get :captionTracks)))))

(aio-defun elfeed-tube-captions--fetch-url (caption-plist)
  (let* ((case-fold-search t)
         (chosen-caption
          (cl-loop
           for lang in elfeed-tube-captions-languages
           for pick = (cl-some
                       (lambda (el) (elfeed-tube-captions--match-lang lang el))
                       caption-plist)
           until pick
           finally return pick))
         base-url language)
    (cond
     ((not caption-plist) (elfeed-tube--message "No captions found!"))
     ((not chosen-caption)
      (elfeed-tube--message
       (format "No captions found in %s!"
               (string-join elfeed-tube-captions-languages ", "))))
     (t (setq base-url (plist-get chosen-caption :baseUrl)
              language (thread-first (plist-get chosen-caption :name)
                                     (plist-get :simpleText)))
        (let* ((response (aio-await (elfeed-tube-curl-enqueue base-url :method "GET")))
               (captions (plist-get response :content))
               (status-code (plist-get response :status-code)))
          (if (= status-code 200)
              captions
            (elfeed-tube--message (plist-get response :error-message))
            (elfeed-tube--message (format "Fetching caption failed with %d" status-code))))))))

(aio-defun elfeed-tube-captions--fetch (entry &optional force-fetch) 
  (if-let* ((_ (not (equal force-fetch '(16))))
            (data-item (elfeed-tube--gethash entry))
            (caption (elfeed-tube-item-caption data-item)))
      caption
    (when-let* ((urls (aio-await (elfeed-tube-captions--fetch-tracks entry)))
                (xmlcaps (aio-await (elfeed-tube-captions--fetch-url urls))))
      (with-temp-buffer
        (insert xmlcaps)
        (goto-char (point-min))
        (dolist (reps '(("&amp;#39;"  . "'")
                        ("&amp;quot;" . "\"")
                        ("\n"         . " ")
                        (" "          . "")))
          (save-excursion
            (while (search-forward (car reps) nil t)
              (replace-match (cdr reps) nil t))))
        (libxml-parse-xml-region (point-min) (point-max))))))

(defun elfeed-tube-captions--show (caption)
  (if  (and (listp caption)
            (eq (car-safe caption) 'transcript))
      (let ((caption-ordered 
             (cl-loop for (_ (start dur) text) in (cddr caption)
                      with pstart = 0
                      for oldtime = 0 then time
                      for time = (string-to-number (cdr start))
                      
                      if (< (mod (floor time) 30) (mod (floor oldtime) 30))
                      collect (list pstart time para) into result and
                      do (setq para nil pstart time)
                      
                      collect (cons time (string-replace "\n" " " text)) into para
                      finally return (nconc result (list (list pstart time para)))))
            (inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize "\n\nTranscript:\n\n"
                            'face 'message-header-name))
        (cl-loop for (start end para) in caption-ordered
                 with beg = (point) do
                 (progn
                   (insert
                    (propertize
                     (format "[%s] - [%s]:\n"
                             (elfeed-tube--timestamp start)
                             (elfeed-tube--timestamp end))
                     'face 'message-header-other)
                    (propertize "\n" 'hard t)
                    (string-join 
                     (mapcar (lambda (tx-cons)
                               (propertize (cdr tx-cons)
                                           'elfeed-tube-timestamp
                                           (car tx-cons)
                                           'face
                                           'variable-pitch
                                           'mouse-face
                                           'highlight
                                           'keymap
                                           elfeed-tube-captions--map))
                             para)
                     " ")
                    (propertize "\n\n" 'hard t)))
                 finally (when-let* ((w shr-width)
                                     (fill-column w)
                                     (use-hard-newlines t))
                           (fill-region beg (point) nil t)))
        (goto-char (point-min)))
    (elfeed-tube--message
     "elfeed-tube-captions--show: No captions available")))

(defvar elfeed-tube-captions--map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'elfeed-tube-captions--follow-event)
    (define-key map (kbd "RET") #'elfeed-tube-captions--follow-link)
    (define-key map [follow-link] 'mouse-face)
    map))

(defun elfeed-tube-captions--follow-event (event)
  (interactive "e")
  (let ((pos (posn-point (event-end event))))
    (elfeed-tube-captions--follow-link pos)))

(defun elfeed-tube-captions--follow-link (pos &optional browser)
  (interactive "d")
  (when-let ((time (get-text-property pos 'elfeed-tube-timestamp))
             (browse-url-browser-function
              (or browser
                  'browse-url-default-browser)))
    (browse-url (concat "https://youtube.com/watch?v="
                        ;; "kaFF1n8ZzaU"
                        (elfeed-tube--get-video-id elfeed-show-entry)
                        "&t="
                        (number-to-string (floor time))))))

(provide 'elfeed-tube-captions)

;; -*- lexical-binding: t; -*-

(require 'pulse)

(defcustom elfeed-tube-mpv-options
  '("--ytdl-format=bestvideo[height<=?480]+bestaudio/best"
    "--cache=yes"
    ;; "--script-opts=osc-scalewindowed=2,osc-visibility=always"
    )
  "List of command line arguments to pass to mpv.

If the mpv library is available, these are appended to
mpv-default-options. Otherwise mpv is started with these options.

Each element in this list is a string. Examples:
- \"--cache=yes\"
- \"--osc=no\""
  :group 'elfeed-tube
  :type '(repeat string))

(defvar elfeed-tube--mpv-available-p
  (and (executable-find "mpv")
       (or (executable-find "youtube-dl")
           (executable-find "yt-dlp"))))
(defvar-local elfeed-tube-mpv--follow-p nil)
(defvar elfeed-tube-mpv--follow-timer nil)
(defvar-local elfeed-tube-mpv--overlay nil)
(defvar elfeed-tube-mpv-hook nil
  "Hook run before starting mpv playback in an elfeed-show buffer.

Each function must accept one argument, the current Elfeed
entry.")

(defsubst elfeed-tube-mpv--check-path (video-url)
  (condition-case nil
      (apply #'string=
             (mapcar
              (lambda (s) 
                (replace-regexp-in-string
                 "&t=[0-9.]*" "" s))
              (list (mpv-get-property "path")
                    video-url)))
    ('error nil)))

(defsubst elfeed-tube-mpv--set-timer (entry)
  (setq elfeed-tube-mpv--follow-timer
        (run-with-timer
         4 1.5 #'elfeed-tube-mpv--follow entry)))

(defsubst elfeed-tube-mpv--overlay-clear ()
  (progn (when (timerp elfeed-tube-mpv--follow-timer)
           (cancel-timer elfeed-tube-mpv--follow-timer))
         (when (overlayp elfeed-tube-mpv--overlay)
           (delete-overlay elfeed-tube-mpv--overlay))))

(defun elfeed-tube-mpv (pos &optional arg)
  (interactive (list (point)
                     current-prefix-arg))
  (if (not elfeed-tube--mpv-available-p)
      (message "Could not find mpv + youtube-dl/yt-dlp in PATH.")
    (when-let* ((time (or (get-text-property pos 'timestamp) 0))
                (video-id (elfeed-tube--get-video-id elfeed-show-entry))
                (video-url (concat "https://youtube.com/watch?v="
                                   video-id
                                   "&t="
                                   (number-to-string (floor time))))
                (args (append elfeed-tube-mpv-options (list video-url)))
                (entry (or elfeed-show-entry
                           (elfeed-search-selected 'ignore-region))))
      (run-hook-with-args 'elfeed-tube-mpv-hook entry)
      ;; (pulse-momentary-highlight-one-line)
      (if (and (not arg) (require 'mpv nil t))
          (if (mpv-live-p)
              (if (elfeed-tube-mpv--check-path video-url)
                  (unless (= 0 time)
                    (mpv-seek time))
                (mpv--enqueue `("loadfile" ,video-url "append")
                              #'ignore)
                (message "Added to playlist: %s"
                         (elfeed-entry-title entry)))
            (apply #'mpv-start args)
            (message
             (concat "Starting mpv: "
                     (propertize "Connected to Elfeed ✓"
                                 'face 'success)))
            (when elfeed-tube-mpv--follow-p
              (elfeed-tube-mpv--set-timer entry)))
        (apply #'start-process
               (concat "elfeed-tube-mpv-"
                       (elfeed-tube--get-video-id elfeed-show-entry))
               nil "mpv" args)
        (message (concat "Starting new mpv instance: "
                         (propertize "Not connected to Elfeed ❌"
                                     'face 'error)))))))

(defun elfeed-tube-mpv--follow (entry-playing)
  (if (not (mpv-live-p))
      (elfeed-tube-mpv--overlay-clear)
    (when-let ((entry-buf (get-buffer
                           (elfeed-show--buffer-name
                            entry-playing))))
      (when (and (or (derived-mode-p 'elfeed-show-mode)
		     (window-live-p (get-buffer-window entry-buf)))
		 (elfeed-tube--same-entry-p
                  (buffer-local-value 'elfeed-show-entry entry-buf)
		  entry-playing)
		 (eq (mpv-get-property "pause")
		     json-false))
	(condition-case nil 
	    (when-let ((mpv-time (mpv-get-property "time-pos")))
	      (with-current-buffer entry-buf

		;; Create overlay
		(unless (overlayp elfeed-tube-mpv--overlay)
		  (save-excursion
		    (beginning-of-buffer)
		    (text-property-search-forward
		     'timestamp)
		    (setq elfeed-tube-mpv--overlay
			  (make-overlay (point) (point)))
		    (overlay-put elfeed-tube-mpv--overlay
				 'face '(:inverse-video t))))
		
                ;; Handle narrowed buffers
                (when (buffer-narrowed-p)
                  (save-excursion
                    (let ((min (point-min))
                          (max (point-max))
                          beg end)
                      (goto-char min)
                      (setq beg (prop-match-value
                                 (text-property-search-forward
                                  'timestamp)))
                      (goto-char max)
                      (widen)
                      (setq end (prop-match-value
                                 (text-property-search-forward
                                  'timestamp)))
                      (narrow-to-region min max)
                      (cond
                       ((and beg (< mpv-time beg))
                        (mpv-set-property "time-pos" (1- beg)))
                       ((and end (> mpv-time end))
                        (mpv-set-property "time-pos" (1+ end))
                        (mpv-set-property "pause" t))))))
                
                ;; Update overlay
                (when-let ((next (elfeed-tube-mpv--where-internal mpv-time)))
                  (goto-char next)
                  (move-overlay elfeed-tube-mpv--overlay
				(save-excursion (beginning-of-visual-line) (point))
				(save-excursion (end-of-visual-line) (point))))))
	  ('error nil))))))

(defun elfeed-tube-mpv--where-internal (mpv-time)
  (save-excursion 
      (while (not (get-text-property (point) 'timestamp))
        (goto-char (or (previous-single-property-change
		        (point) 'timestamp)
		       (next-single-property-change
		        (point) 'timestamp))))

      (if (> (get-text-property (point) 'timestamp)
	     mpv-time)
	  (let ((match (text-property-search-backward
		        'timestamp mpv-time
		        (lambda (mpv cur)
			  (< (or cur
			         (get-text-property
				  (1+ (point))
				  'timestamp))
			     (- mpv 1))))))
	    (goto-char (prop-match-end match))
	    (text-property-search-forward 'timestamp)
	    (min (1+ (point)) (point-max)))
        (let ((match (text-property-search-forward
		      'timestamp mpv-time
		      (lambda (mpv cur) (if cur (> cur (- mpv 1)))))))
          (prop-match-beginning match)))))

(defun elfeed-tube-mpv-where ()
  "Jump to the current position of mpv playback."
  (interactive)
  (cond
   ((not (featurep 'mpv))
    (message "mpv-where requires the mpv package. You can install it with M-x `package-install' RET mpv RET."))
   ((not (and (derived-mode-p 'elfeed-show-mode)
              (elfeed-tube--youtube-p elfeed-show-entry)))
    (message "Not in an elfeed-show buffer for a Youtube video!"))
   ((not (mpv-live-p))
    (message "No running instance of mpv is connected to Emacs."))
   ((or (previous-single-property-change
	 (point) 'timestamp)
	(next-single-property-change
	 (point) 'timestamp))
    (goto-char (elfeed-tube-mpv--where-internal
                (mpv-get-property "time-pos")))
    (let ((pulse-delay 0.08)
          (pulse-iterations 16))
      (pulse-momentary-highlight-one-line)))
   (t (message "Transcript location not found in buffer."))))

(define-minor-mode elfeed-tube-mpv-follow-mode
  "Toggle mpv follow mode in elfeed-show buffers that display
Youtube feed entries. When the video player mpv is started from
this buffer (from any location in the transcript), turning on
this minor-mode will cause the cursor to track the currently
playing segment in mpv. You can still click anywhere in the
transcript to seek to that point in the video."
  :global nil
  :version "0.10"
  :lighter "(-->)"
  :keymap (let ((map (make-sparse-keymap)))
            (prog1 map
              (define-key map " " #'mpv-pause)))
  :group 'elfeed-tube
  (if elfeed-tube-mpv-follow-mode
      (cond
       
       ((not (require 'mpv nil t))
        (message "mpv-follow-mode requires the mpv package. You can install it with M-x `package-install' RET mpv RET.")
        (elfeed-tube-mpv-follow-mode -1))
       
       ((not (derived-mode-p 'elfeed-show-mode))
        (message "mpv-follow-mode only works in elfeed-show buffers.")
        (elfeed-tube-mpv-follow-mode -1))
       
       (t (if-let* ((entry elfeed-show-entry)
                    (video-id (elfeed-tube--get-video-id entry))
                    (video-url
                     (concat "https://youtube.com/watch?v="
                             video-id)))
              (if (and (mpv-live-p) (elfeed-tube-mpv--check-path video-url))
                  (elfeed-tube-mpv--set-timer entry)
                (setq-local elfeed-tube-mpv--follow-p t))
            (message "Not a youtube video buffer!")
            (elfeed-tube-mpv-follow-mode -1))))
    
    (setq-local elfeed-tube-mpv--follow-p nil)
    (when (timerp elfeed-tube-mpv--follow-timer)
      (cancel-timer elfeed-tube-mpv--follow-timer))
    (elfeed-tube-mpv--overlay-clear)))

(provide 'elfeed-tube-mpv)

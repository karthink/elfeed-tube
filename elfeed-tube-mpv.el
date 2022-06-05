;; -*- lexical-binding: t; -*-

(defvar elfeed-tube--mpv-available-p
  (and (executable-find "mpv")
       (or (executable-find "youtube-dl")
           (executable-find "yt-dlp"))))
(defvar-local elfeed-tube-mpv--follow-p nil)
(defvar elfeed-tube-mpv--follow-timer nil)
(defvar-local elfeed-tube-mpv--overlay nil)
(defvar elfeed-tube-mpv-play-hook nil
  "Hook run before starting mpv playback in an elfeed-show buffer.

Each function must accept one argument, the current Elfeed
entry.")

(define-key elfeed-show-mode-map
  (kbd "C-c C-f") #'elfeed-tube-mpv-follow-mode)

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

(defun elfeed-tube-mpv-play (pos)
  (interactive "d")
  (if (not elfeed-tube--mpv-available-p)
      (message "Could not find mpv + youtube-dl/yt-dlp in PATH.")
    (let* ((time (get-text-property pos 'elfeed-tube-timestamp))
           (video-id (elfeed-tube--get-video-id elfeed-show-entry))
           (video-url (concat "https://youtube.com/watch?v="
                              video-id
                              "&t="
                              (number-to-string (floor time))))
           (args `("--ytdl-format=bestvideo[height<=?480]+bestaudio/best"
                   "--cache=yes"
                   ,video-url))
           (entry elfeed-show-entry))
      (run-hook-with-args 'elfeed-tube-mpv-play-hook entry)
      (if (require 'mpv nil t)
          (if (and (mpv-live-p)
                   (elfeed-tube-mpv--check-path video-url))
              (mpv-seek time)
            (apply #'mpv-start args)
            (when elfeed-tube-mpv--follow-p
              (elfeed-tube-mpv--set-timer entry)))
        (apply #'start-process
               (concat "elfeed-tube-mpv-"
                       (elfeed-tube--get-video-id elfeed-show-entry))
               nil "mpv" args)))))

(defun elfeed-tube-mpv--follow (entry-playing)
  (if (not (mpv-live-p))
      (elfeed-tube-mpv--overlay-clear)
    (when-let ((entry-buf (get-buffer "*elfeed-entry*")))
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
			 (while (not (get-text-property (point) 'elfeed-tube-timestamp))
			   (goto-char (or (previous-single-property-change
					   (point) 'elfeed-tube-timestamp)
					  (next-single-property-change
					   (point) 'elfeed-tube-timestamp))))
			 
			 ;; Create overlay
			 (unless (overlayp elfeed-tube-mpv--overlay)
			   (save-excursion
			     (beginning-of-buffer)
			     (text-property-search-forward
			      'elfeed-tube-timestamp)
			     (setq elfeed-tube-mpv--overlay
				   (make-overlay (point) (point)))
			     (overlay-put elfeed-tube-mpv--overlay
					  'face '(:inverse-video t))))
			 
			 ;; Update overlay
			 (if (> (get-text-property (point) 'elfeed-tube-timestamp)
				mpv-time)
			     (let ((match (text-property-search-backward
					   'elfeed-tube-timestamp mpv-time
					   (lambda (mpv cur)
					     (< (or cur
						    (get-text-property
						     (1+ (point))
						     'elfeed-tube-timestamp))
						(- mpv 1))))))
			       (goto-char (prop-match-end match))
			       (text-property-search-forward 'elfeed-tube-timestamp)
			       (forward-char 1))
			   (let ((match (text-property-search-forward
					 'elfeed-tube-timestamp mpv-time
					 (lambda (mpv cur) (if cur (> cur (- mpv 1)))))))
			     (goto-char (prop-match-beginning match))))
			 
			 (move-overlay elfeed-tube-mpv--overlay
				       (save-excursion (beginning-of-visual-line) (point))
				       (save-excursion (end-of-visual-line) (point)))))
		   ('error nil))))))

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

;; -*- lexical-binding: t; -*-
(require 'rx)
(require 'aio)
(require 'url)

(defvar elfeed-tube-captions-puntcuate-p nil)

(aio-defun elfeed-tube--punctuate-captions (xmlcaps)
  "Accept libxml transcript representation,
return punctuated version"
  ;; (pp-display-expression xmlcaps (get-buffer-create "xmlcaps-buf"))
  (let* ((all-words)
         (word-lengths 
          (cl-loop for (_ (st dt) tx) in (cddr xmlcaps)
                   do (push (string-trim tx) all-words)
                   sum (length (split-string (string-trim tx) " " t))
                   into cumlen
                   collect cumlen))
         (response (aio-await (elfeed-tube-curl-enqueue
                     "http://bark.phon.ioc.ee/punctuator"
                     :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                     :method "POST"
                     :data (concat "text="
                                   (-> all-words
                                       (nreverse)
                                       (string-join " ")
                                       (url-hexify-string))))))
         (status-code (plist-get response :status-code))
         (punctuated (plist-get response :content)))
    (if (= status-code 200)
        (progn
          (cl-loop for num in word-lengths
                   for (type (st dt) tx) in (cddr xmlcaps)
                   with high = (car (last word-lengths))
                   with captions
                   with tokens = (split-string (elfeed-tube--preprocess-captions punctuated) " " t)
                   for prev = 0 then len
                   for len = num
                   collect `(,type
                             (,st ,dt)
                             ,(string-join (cl-subseq tokens prev (unless (= len high) len)) " "))
                   into captions
                   finally return (nconc '(transcript nil) captions)))
      (message (plist-get response :error-messsage))
      nil)))

(defun elfeed-tube--preprocess-captions (captions)
  (with-temp-buffer
    (insert captions)
    (goto-char (point-min))
    (dolist (reps `((,(rx (and (group (syntax open-parenthesis)) (one-or-more (or space punct)))) . "\\1")
                    (,(rx (and (one-or-more (or space punct)) (group (syntax close-parenthesis)))) . "\\1")
                    (,(rx " i" (group (or punct space))) . " I\\1")
                    ("'T" . "'t") ("'S" . "'s") ("'D" . "'d") ("'M" . "'m")))
      (save-excursion
        (while (re-search-forward (car reps) nil t)
          (replace-match (cdr reps)))))
    (buffer-string)))

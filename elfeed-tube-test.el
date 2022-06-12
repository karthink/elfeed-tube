(require 'ert)
(require 'elfeed-tube)

(defsubst msg (message)
  (prin1 message (get-buffer "*scratch*")))

(defmacro etd (&rest body)
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

(defmacro etdb (&rest body)
  (declare (indent defun))
  `(let ((entry (if (buffer-live-p (get-buffer "*elfeed-entry*"))
                    (buffer-local-value
                     'elfeed-show-entry
                     (get-buffer "*elfeed-entry*"))
                  (unless (buffer-live-p (get-buffer "*elfeed-search*"))
                    (save-window-excursion (elfeed-search)))
                  (with-current-buffer (get-buffer "*elfeed-search*")
                    (elfeed-search-selected 'no-region))))
         (elfeed-db-directory elfeed-tube--captions-db-dir))
     ,@body))

(ert-deftest elfeed-tube--health-test ()
  "Check if the local environment satisfies requirements for Elfeed
Tube."
  (should (executable-find "curl"))
  (should (featurep 'elfeed))
  (should (featurep 'aio))
  (message "Elfeed tube requirements satisfied")
  
  (should (executable-find "mpv"))
  (should (featurep 'mpv))
  (should (or (executable-find "youtube-dl")
              (executable-find "yt-dlp")))
  (message "Elfeed tube mpv requirements satisfied"))

(ert-deftest elfeed-tube--persist-test ()
  "Test if data is persistent as expected."
  (skip-unless (equal system-name "t143"))
  (dolist (id '(("www.youtube.com" . "yt:video:SSg3T0aK-ck")
                ("www.youtube.com" . "yt:video:nGlhMk1hEZw")
                ("www.youtube.com" . "yt:video:_zJbi9YatcA")
                ("www.youtube.com" . "yt:video:6kolTgj7uQc")
                ("www.youtube.com" . "yt:video:19o8yPaVp58")
                ("www.youtube.com" . "yt:video:swLyst02ZK4")
                ("www.youtube.com" . "yt:video:N3Qlqv2vm0g")))
    (let ((entry (elfeed-db-get-entry id)))
      (should-not (null (elfeed-entry-content entry)))
      (should-not (null (elfeed-meta entry :caps)))
      (should-not (null (elfeed-meta entry :thumb)))))
  
  (dolist (id '(("www.youtube.com" . "yt:video:3fJhTu-0zmo")
                ("www.youtube.com" . "yt:video:uXlQuTRSmzc")))
    (let ((entry (elfeed-db-get-entry id)))
      (should (null (elfeed-entry-content entry)))
      (should (null (elfeed-meta entry :caps)))
      (should (null (elfeed-meta entry :thumb))))))

(ert-deftest elfeed-tube--channel-add-predicate-test ()
  "Test if input urls to find youtube channels are matched correctly."
  (let ((vid-url-1 "https://www.youtube.com/watch?v=IV3dnLzthDA&t=120")
        (vid-url-2 "https://youtu.be/IV3dnLzthDA&t=10")
        (play-url "https://www.youtube.com/playlist?list=PLexa_cotS5sHIkALAXxPHyMm5LA2kQ5DV&t=10")
        (chan-url-2 "https://www.youtube.com/c/veritasium")
        (chan-url-1 "https://www.youtube.com/channel/UCHnyfMqiRRG1u-2MsSQLbXA"))
    (should (and (elfeed-tube--video-p vid-url-1)
                 (string= (match-string 1 vid-url-1) "IV3dnLzthDA")))
    (should (and (elfeed-tube--video-p vid-url-2)
                 (string= (match-string 1 vid-url-2) "IV3dnLzthDA")))
    (should (and (elfeed-tube--playlist-p play-url)
                 (string= (match-string 1 play-url)
                          "PLexa_cotS5sHIkALAXxPHyMm5LA2kQ5DV")))
    (should (and (elfeed-tube--channel-p chan-url-1)
                 (string= (match-string 1 chan-url-1)
                          "UCHnyfMqiRRG1u-2MsSQLbXA")))))

(ert-deftest elfeed-tube--desc-fetch-test ()
  (let* ((elfeed-tube-invidious-url "https://vid.puffyan.us")
         (elfeed-tube-fields '(duration thumbnail description))
         (elfeed-tube-thumbnail-size 'small)
         (entry (elfeed-entry--create
                 :id '("www.youtube.com" . "yt:video:Pj-h6MEgE7I")
                 :title "You Are Not Where You Think You Are"
                 :link "https://www.youtube.com/watch?v=Pj-h6MEgE7I"
                 :date 1652795984.0
                 :tags '(youtube)
                 :feed-id "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q"
                 :content-type 'html
                 :meta '(:authors
                         ((:name "Kurzgesagt – In a Nutshell" :uri "https://www.youtube.com/channel/UCsXVk37bltHxD1rDPwtNM8Q")))))
         (desc (aio-wait-for (elfeed-tube--fetch-desc entry 1))))
    (should (listp desc))
    (should (= (floor (plist-get desc :length)) 595))
    (should (stringp (plist-get desc :thumb)))
    (should (string= (substring (plist-get desc :desc) nil 10) "Start your"))))

(ert-deftest elfeed-tube--caps-fetch-test ()
  "Test if captions are retrieved correctly for Youtube videos."
  (let* ((entry
         (elfeed-entry--create
          :id '("www.youtube.com" . "yt:video:Pj-h6MEgE7I")
          :title "You Are Not Where You Think You Are"
          :link "https://www.youtube.com/watch?v=Pj-h6MEgE7I"
          :date 1652795984.0
          :tags '(youtube)
          :feed-id "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q"
          :content-type 'html
          :meta '(:authors
                  ((:name "Kurzgesagt – In a Nutshell" :uri "https://www.youtube.com/channel/UCsXVk37bltHxD1rDPwtNM8Q")))))
         (caption-tracks (aio-wait-for (elfeed-tube--fetch-captions-tracks entry))))
    
    (should-not (cl-set-difference
                 (mapcar (lambda (it) (plist-get it :languageCode))
                         caption-tracks)
                 '("bg" "en" "en" "it" "ja" "ko" "pt-BR" "th" "tr")
                 :test #'string=))

    (should-not (cl-set-difference
                 (mapcar (lambda (it) (plist-get
                                       (plist-get it :name)
                                       :simpleText))
                         caption-tracks)
                 '("Bulgarian" "English"
                   "English (auto-generated)"
                   "Italian" "Japanese"
                   "Korean" "Turkish"
                   "Thai" "Portuguese (Brazil)")
                 :test #'string=))
    
    (let* ((elfeed-tube-captions-languages '("english" "english (auto generated)"))
           (caption-xml (aio-wait-for (elfeed-tube--fetch-captions-url
                                       caption-tracks entry)))
           caption-sexp)
      (should (string= (car caption-xml) "English"))
      (should (and (stringp (cdr caption-xml))
                   (string= (substring (cdr caption-xml) nil 14)
                            "<?xml version=")))
      (setq caption-sexp
            (with-temp-buffer
              (save-excursion (insert (cdr caption-xml)))
              (libxml-parse-xml-region (point-min) (point-max))))

      (should-not  (cl-mismatch (cl-subseq caption-sexp 0 2)
                                '(transcript nil))))))
  
(provide 'elfeed-tube-test)

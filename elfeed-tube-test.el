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
  (message "Elfeed tube requirements satisfied"))

(ert-deftest elfeed-tube-mpv--health-test ()
  "Check if the local environment satisfies requirements for Elfeed
Tube with mpv."
  (should (executable-find "mpv"))
  (should (featurep 'mpv))
  (should (featurep 'elfeed-tube))
  (should (or (executable-find "youtube-dl")
              (executable-find "yt-dlp")))
  (message "Elfeed tube mpv requirements satisfied"))

(ert-deftest elfeed-tube--persist-test ()
  "Test if data is persistent as expected."
  (skip-unless (equal system-name "t14"))
  (cl-loop for id in '(("www.youtube.com" . "yt:video:_bJeKUosqoY")
                       ("www.youtube.com" . "yt:video:SSg3T0aK-ck")
                       ("www.youtube.com" . "yt:video:swLyst02ZK4"))
           for file in '("9d52d348f4ca01d6b6a72e4e27e48c51d5305c21"
                         "9a540de06a0d0d2d6fbafc3cff1a191c2263e5d9"
                         "8650958b89cbeee1fce8d13aa656829e91278621")
           for pre = (substring file 0 2)
           ;; Files should exist
           do
           (should (file-exists-p
                    (file-name-concat
                     elfeed-db-directory "data" pre file)))
           do
           (let ((entry (elfeed-db-get-entry id)))
             ;; Ref should exist in index
             (should-not (null (elfeed-entry-content entry)))
             (should (string= file
                              (elfeed-ref-id (elfeed-entry-content entry))))
             
             ;; Caps and thumbnail should exist
             (should-not (null (elfeed-meta entry :caps)))
             (let ((cap-file (elfeed-ref-id 
                              (elfeed-meta entry :caps))))
               (should (file-exists-p
                        (file-name-concat
                         elfeed-tube--captions-db-dir "data"
                         (substring cap-file 0 2) cap-file))))
             
             (should (numberp (elfeed-meta entry :duration)))
             (should-not (null (elfeed-meta entry :thumb)))))
  
  (dolist (id '(("www.youtube.com" . "yt:video:3fJhTu-0zmo")
                ("www.youtube.com" . "yt:video:uXlQuTRSmzc")))
    (let ((entry (elfeed-db-get-entry id)))
      (should (null (elfeed-entry-content entry)))
      (should (null (elfeed-meta entry :caps)))
      (should (null (elfeed-meta entry :thumb))))))

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

(ert-deftest elfeed-tube--fetch-1-test ()
  "Fetch all available data for an Elfeed entry"
  (let* ((entry (elfeed-entry--create
                 :id '("www.youtube.com" . "yt:video:Pj-h6MEgE7I")
                 :title "You Are Not Where You Think You Are"
                 :link "https://www.youtube.com/watch?v=Pj-h6MEgE7I"
                 :date 1652795984.0
                 :tags '(youtube)
                 :feed-id "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q"
                 :content-type 'html
                 :meta '(:authors
                         ((:name "Kurzgesagt – In a Nutshell" :uri "https://www.youtube.com/channel/UCsXVk37bltHxD1rDPwtNM8Q")))))
         (elfeed-tube-field '(duration thumbnail description captions))
         (elfeed-tube-auto-save-p nil))
    
    (when (elfeed-tube--gethash entry)
      (remhash (elfeed-tube--entry-video-id entry) elfeed-tube--info-table))

    (aio-wait-for (elfeed-tube--fetch-1 entry))

    (let ((cached (elfeed-tube--gethash entry)))
      (should (elfeed-tube-item-p cached))
      (should-not (elfeed-tube-item-error cached))
      (should (elfeed-tube-item-thumb cached))
      (should (elfeed-tube-item-desc cached))
      (should (elfeed-tube-item-len cached))
      (should (equal (cl-subseq (elfeed-tube-item-caps cached) 0 2)
                     '(transcript nil))))))
  
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

(ert-deftest elfeed-tube--channel-add-test ()
  "Check if Youtube channel feed URLs are being discovered.

This checks against all supported kinds of queries"
  (let ((channels
       (aio-wait-for 
        (elfeed-tube-add--get-channels
         '("https://www.youtube.com/c/RealEngineering"
           "prozd"
           "https://www.youtube.com/channel/UCP3alkHEJhoxTpy3iVON-lg"
           "short circuit"
           "https://www.youtube.com/watch?v=_yMMTVVJI4c"
           "neovim"
           "https://www.youtube.com/playlist?list=PLTZM4MrZKfW-ftqKGSbO-DwDiOGqNmq53")))))
  
  (should-not
   (cl-set-difference
    (mapcar (lambda (c) (elfeed-tube-channel-feed c)) channels)
    '("https://www.youtube.com/feeds/videos.xml?channel_id=UCy0tKL1T7wFoYcxCe0xjN6Q"
      "https://www.youtube.com/feeds/videos.xml?channel_id=UC6MFZAOHXlKK1FI7V0XQVeA"
      "https://www.youtube.com/feeds/videos.xml?channel_id=UCS97tchJDq17Qms3cux8wcA"
      "https://www.youtube.com/feeds/videos.xml?channel_id=UCR1IuLEqb6UEA_zQ81kwXfg"
      "https://www.youtube.com/feeds/videos.xml?playlist_id=PLTZM4MrZKfW-ftqKGSbO-DwDiOGqNmq53"
      "https://www.youtube.com/feeds/videos.xml?channel_id=UCdBK94H6oZT2Q7l0-b0xmMg"
      "https://www.youtube.com/feeds/videos.xml?channel_id=UCP3alkHEJhoxTpy3iVON-lg")
    :test 'string=))
  
  (should-not
   (cl-set-difference
    (mapcar (lambda (c) (elfeed-tube-channel-url c)) channels)
    '("https://www.youtube.com/channel/UCy0tKL1T7wFoYcxCe0xjN6Q"
     "https://www.youtube.com/channel/UC6MFZAOHXlKK1FI7V0XQVeA"
     "https://www.youtube.com/channel/UCS97tchJDq17Qms3cux8wcA"
     "https://www.youtube.com/channel/UCR1IuLEqb6UEA_zQ81kwXfg"
     "https://www.youtube.com/playlist?list=PLTZM4MrZKfW-ftqKGSbO-DwDiOGqNmq53"
     "https://www.youtube.com/channel/UCdBK94H6oZT2Q7l0-b0xmMg"
     "https://www.youtube.com/channel/UCP3alkHEJhoxTpy3iVON-lg")
    :test 'string=))

  (should-not
   (cl-set-difference
    (mapcar (lambda (c) (elfeed-tube-channel-author c)) channels)
    '("Technology Connections"
      "ProZD"
      "chris@machine"
      "Real Engineering"
      "Electrical Grid"
      "ShortCircuit"
      "Physics with Elliot")
    :test 'string=))))

(provide 'elfeed-tube-test)

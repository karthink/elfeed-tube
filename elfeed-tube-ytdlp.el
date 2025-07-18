;;; elfeed-tube-ytdlp.el --- yt-dlp support for elfeed-tube  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: hypermedia

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

;; yt-dlp support for elfeed-tube, loaded only if `elfeed-tube-backend' is set
;; to yt-dlp.

;;; Code:
(require 'aio)

(defvar elfeed-tube-thumbnail-size)
(declare-function elfeed-tube--entry-video-id "elfeed-tube")
(declare-function elfeed-tube-include-p "elfeed-tube")
(declare-function elfeed-tube-log "elfeed-tube")
(declare-function elfeed-tube--truncate "elfeed-tube")
(declare-function elfeed-entry-title "elfeed-tube")
(declare-function elfeed-tube-curl-enqueue "elfeed-tube")
(declare-function elfeed-tube--fetch-captions-sblock "elfeed-tube")
(declare-function elfeed-tube--sblock-captions "elfeed-tube")
(declare-function elfeed-tube--npreprocess-captions "elfeed-tube")
(defvar elfeed-tube-captions-puntcuate-p)
(defvar elfeed-tube-captions-sblock-p)
(defvar elfeed-tube-captions-languages)

(defconst elfeed-tube--ytdlp-thumb-sizes
  '(large 480 medium 360 small 188)
  "Mapping from elfeed-tube thumbnail sizes to standard yt-dlp
thumbnail image heights")

(aio-defun elfeed-tube--ytdlp-fetch (url)
  "Return a hash table of the JSON dump as retrieved by yt-dlp.

URL is the video id or url.  The data is cached in a global hash
table."
  (unless (executable-find "yt-dlp")
    (user-error
     "Could not find yt-dlp executable. Please install yt-dlp or add to path."))
  (let* ((yt-proc
          (start-process
           "yt-dlp" (get-buffer-create url)
           "yt-dlp" "--quiet" "--skip-download" "--dump-json" url))
         (promise (aio-promise)))
    (set-process-query-on-exit-flag yt-proc nil)
    (set-process-sentinel
     yt-proc (lambda (_ status) (aio-resolve promise (lambda () status))))
    (aio-await promise)
    ;; TODO: Handle json-parse-error
    (prog1 (ignore-errors
             (with-current-buffer url
               (goto-char (point-min))
               (when (re-search-forward "^{" nil t)
                 (forward-line 0)
                 (json-parse-buffer :object-type 'plist))))
      (kill-buffer url))))

(defun elfeed-tube--ytdlp-get-chapters (chapter-data)
  "Convert list of hashtables of chapter information obtained from
yt-dlp JSON dump into alist format consumed by the rest of
elfeed-tube."
  (cl-loop for chapter in chapter-data
           for title = (plist-get chapter :title)
           for start = (number-to-string (floor (plist-get chapter :start_time)))
           collect (cons start title)))

(defun elfeed-tube--ytdlp-get-thumb (thumb-data)
  "Take list of hashtables of video thumbnails obtained from yt-dlp
json dump and return the url for the thumbnail of required size."
  (cl-loop with height = (plist-get elfeed-tube--ytdlp-thumb-sizes
                                    elfeed-tube-thumbnail-size)
           for thumb-plist across thumb-data
           if (equal (plist-get thumb-plist :height) height)
           return (plist-get thumb-plist :url)))

;; Main yt-dlp call and response handling
(aio-defun elfeed-tube--ytdlp-fetch-desc (entry &optional _attempts)
  "Return a plist containing description, duration, thumbnail
 url and chapter data for elfeed ENTRY."
  (let* ((video-id (elfeed-tube--entry-video-id entry))
         (videodata (aio-await (elfeed-tube--ytdlp-fetch video-id)))
         caps)
    (if videodata
        (progn
          
          (when (elfeed-tube-include-p 'captions)
            (if-let* ((caption-tracks
                       (nconc (plist-get videodata :subtitles)
                              (plist-get videodata :automatic_captions))))
                (setq caps (aio-await (elfeed-tube--ytdlp-fetch-captions
                                       caption-tracks entry)))
              (elfeed-tube-log 'warn "[Captions][video:%s]: Not available"
                               (elfeed-tube--truncate (elfeed-entry-title entry)))))
          
          (nconc
           (list :length (plist-get videodata :duration)
                 :thumb (elfeed-tube--ytdlp-get-thumb
                         (plist-get videodata :thumbnails))
                 :desc (replace-regexp-in-string
                        "\n" "<br>" (plist-get videodata :description))
                 :chaps (elfeed-tube--ytdlp-get-chapters
                         (plist-get videodata :chapters)))
           (and caps (list :caps caps))))
      (prog1 nil
        (elfeed-tube-log 'error
                         "[Description][video:%s]: %s"
                         (elfeed-tube--truncate (elfeed-entry-title entry))
                         "Could not fetch video data using yt-dlp")
        (message "Error fetching video data. See log.")))))

(aio-defun elfeed-tube--ytdlp-fetch-captions-url (caption-plist entry)
  (let* ((case-fold-search t)
         (chosen-caption
          (cl-loop
           for lang in elfeed-tube-captions-languages
           for arr = (plist-get
                      caption-plist
                      (intern (concat ":" (car (split-string lang)))))
           if (cl-some (lambda (type-plist)
                         (and (equal (plist-get type-plist :ext) "srv3")
                              type-plist))
                       arr)
           return it))
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
                        "&fmt=srv3" "" (plist-get chosen-caption :url))
              language (plist-get chosen-caption :name))
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

(aio-defun elfeed-tube--ytdlp-fetch-captions (urls entry)
  (pcase-let* ((`(,language . ,xmlcaps)
                (aio-await (elfeed-tube--ytdlp-fetch-captions-url urls entry)))
               (sblock (and elfeed-tube-captions-sblock-p
                            (aio-await (elfeed-tube--fetch-captions-sblock entry))))
               (parsed-caps))
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

(provide 'elfeed-tube-ytdlp)
;;; elfeed-tube-ytdlp.el ends here

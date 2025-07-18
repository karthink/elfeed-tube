;;; elfeed-tube-invidious.el --- invidious support for elfeed-tube  -*- lexical-binding: t; -*-

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

;; 

;;; Code:
(require 'aio)
(require 'elfeed)
(require 'elfeed-tube-utils)

(defcustom elfeed-tube-invidious-url nil
  "Invidious URL to use for retrieving data.

Setting this is optional: If left unset, elfeed-tube will locate
and use an Invidious URL at random. This should be set to a
string, for example \"https://invidio.us\"."
  :group 'elfeed-tube
  :type '(choice (string :tag "Custom URL")
          (const :tag "Disabled (Auto)" nil)))

(defvar elfeed-tube--invidious-servers nil)

(defvar elfeed-tube--invidious-api-videos-path "/api/v1/videos/")
(defvar elfeed-tube--invidious-api-video-fields
  '("videoThumbnails" "description" "lengthSeconds"))

(aio-defun elfeed-tube--aio-fetch (url &optional next desc attempts)
  "Fetch URL asynchronously using `elfeed-curl-retrieve'.

If successful (HTTP 200), return the JSON-parsed result as a
plist.

Otherwise, call the function NEXT (with no arguments) and try
ATTEMPTS more times. Return nil if all attempts fail. DESC is a
description string to print to the elfeed-tube log allong with
any other error messages.

This function returns a promise."
  (let ((attempts (or attempts (1+ elfeed-tube--max-retries))))
    (when (> attempts 0)
      (let* ((response
              (aio-await (elfeed-tube-curl-enqueue url :method "GET")))
             (content (plist-get response :content))
             (status (plist-get response :status-code))
             (error-msg (plist-get response :error-message)))
        (cond
         ((equal status 200)
          (condition-case nil
              (json-parse-string content :object-type 'plist)
            ((json-parse-error error)
             (elfeed-tube-log 'error "[Search] JSON malformed (%s)"
                              (elfeed-tube--attempt-log attempts))
             (and (functionp next) (funcall next))
             (aio-await
              (elfeed-tube--aio-fetch url next desc (1- attempts))))))
         (t (elfeed-tube-log 'error "[Search][%s]: %s (%s)" error-msg url
                             (elfeed-tube--attempt-log attempts))
            (and (functionp next) (funcall next))
            (aio-await
             (elfeed-tube--aio-fetch url next desc (1- attempts)))))))))

(aio-defun elfeed-tube--get-invidious-servers ()
  (let* ((instances-url (concat "https://api.invidious.io/instances.json"
                                "?pretty=1&sort_by=type,users"))
         (result (aio-await (elfeed-tube-curl-enqueue instances-url :method "GET")))
         (status-code (plist-get result :status-code))
         (servers (plist-get result :content)))
    (when (equal status-code 200)
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

(declare-function elfeed-tube--get-chapters "elfeed-tube")
(defvar elfeed-tube-thumbnail-size)

(defun elfeed-tube--invidious-parse-desc (api-data)
  "Parse API-DATA for video description."
  (let* ((length-seconds (plist-get api-data :lengthSeconds))
         (desc-html (plist-get api-data :description))
         (chapters (elfeed-tube--get-chapters desc-html))
         (desc-html (replace-regexp-in-string
                     "\n" "<br>"
                     desc-html))
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
    (list :length length-seconds :thumb thumb :desc desc-html :chaps chapters)))

(defsubst elfeed-tube--nrotate-invidious-servers ()
  "Rotate the list of Invidious servers in place."
  (setq elfeed-tube--invidious-servers
        (nconc (cdr elfeed-tube--invidious-servers)
               (list (car elfeed-tube--invidious-servers)))))

(aio-defun elfeed-tube--invidious-fetch-desc (entry &optional existing attempts)
  (let* ((attempts (or attempts (1+ elfeed-tube--max-retries)))
         (video-id (elfeed-tube--entry-video-id entry)))
    (when (> attempts 0)
      (if-let ((invidious-url (aio-await (elfeed-tube--get-invidious-url))))
          (let* ((api-url (concat
                           invidious-url
                           elfeed-tube--invidious-api-videos-path
                           video-id
                           "?fields="
                           (string-join elfeed-tube--invidious-api-video-fields ",")))
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
            (if (equal api-status 200)
                ;; Return data
                (condition-case error
                    (prog1
                        (elfeed-tube--invidious-parse-desc
                         (json-parse-string api-data :object-type 'plist)))
                  (json-parse-error
                   (elfeed-tube-log
                    'error
                    "[Description][video:%s]: JSON malformed %s"
                    (elfeed-tube--truncate (elfeed-entry-title entry))
                    (elfeed-tube--attempt-log attempts))
                   (elfeed-tube--nrotate-invidious-servers)
                   (aio-await
                    (elfeed-tube--invidious-fetch-desc entry existing (- attempts 1)))))
              ;; Retry #attempts times
              (elfeed-tube-log 'error
                               "[Description][video:%s][%s]: %s %s"
                               (elfeed-tube--truncate (elfeed-entry-title entry))
                               api-url
                               (plist-get api-response :error-message)
                               (elfeed-tube--attempt-log attempts))
              (elfeed-tube--nrotate-invidious-servers)
              (aio-await
               (elfeed-tube--invidious-fetch-desc entry existing (- attempts 1)))))

        (message
         "Could not find a valid Invidious url. Please customize `elfeed-tube-invidious-url'.")
        nil))))

(provide 'elfeed-tube-invidious)
;;; elfeed-tube-invidious.el ends here

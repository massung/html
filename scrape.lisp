;;;; HTML Parsing and Rendering for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(in-package :html)

;;; ----------------------------------------------------

(defun html-scrape (html)
  "Scrape HTML in s-expression form for text, links, and media."
  (let (links images videos (text (make-string-output-stream)))
    (labels ((scrape (form)
               (when (listp form)
                 (destructuring-bind (tag &optional atts &rest forms)
                     form

                   ;; dive into the body tag
                   (cond ((string-equal tag "body")
                          (mapc #'scrape forms))

                         ;; links
                         ((string-equal tag :a)
                          (let ((href (assoc :href atts :test #'string-equal)))
                            (when href
                              (push (second href) links))))

                         ;; images
                         ((string-equal tag :img)
                          (let ((src (assoc :src atts :test #'string-equal)))
                            (when src
                              (push (second src) images))))

                         ;; if it's another tag, dive down into it
                         (t (mapc #'scrape forms)))))))
      (scrape html))
    `((:links ,@links) (:images ,@images) (:videos ,@videos))))

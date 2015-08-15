;;;; Simple HTML Rendering for Common Lisp
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

(defpackage :html
  (:use :cl)
  (:export
   #:html
   #:html-format
   #:html-encode))

(in-package :html)

;;; ----------------------------------------------------

(defparameter *encode-html-p* t
  "T if forms that are rendered as HTML should be encoded.")

;;; ----------------------------------------------------

(defconstant +tag-format+
  "<~a~:{ ~a~@[='~:/html-format/'~]~}>~{~/html-format/~}~:[</~a>~;~]"
  "Format for rendering a tag string.")

;;; ----------------------------------------------------

(defconstant +singleton-tags+ '(:area :base :br :col :command :embed :hr :img :input :link :meta :param :source)
  "Tags that do not have matching close tags.")

;;; ----------------------------------------------------

(defconstant +lang-tags+ '(:style :script)
  "Tags that do not encode their inner HTML text.")

;;; ----------------------------------------------------

(defun html (stream form)
  "Renders HTML to a string."
  (if (null stream)
      (with-output-to-string (s)
        (html-format s form))
    (html-format stream form)))

;;; ----------------------------------------------------

(defun html-format (stream form &optional colonp atp &rest args)
  "Output an form to a stream as HTML."
  (declare (ignore atp args))
  (if (listp form)
      (destructuring-bind (tag &optional atts &rest body)
          form

        ;; only encode forms that are not special language tags
        (let ((*encode-html-p* (or colonp (not (find tag +lang-tags+)))))
          (format stream
                  +tag-format+

                  ;; output the tag, attributes and body
                  tag
                  atts
                  body

                  ;; no close tag if a singleton tag
                  (find tag +singleton-tags+)

                  ;; close tag
                  tag)))

    ;; not a tag form, so just print it to the stream
    (if (or *encode-html-p* colonp)
        (html-encode stream (princ-to-string form))
      (princ form stream))))

;;; ----------------------------------------------------

(defun html-encode (stream string)
  "Replace characters from HTML to &entity; references."
  (flet ((encode (c)
           (case c
             (#\" "&quot;")
             (#\' "&apos;")
             (#\< "&lt;")
             (#\> "&gt;")
             (#\& "&amp;")

             ;; non-ascii characters
             (otherwise (let ((n (char-code c)))
                          (if (<= 32 n 127)
                              c
                            (format nil "&#~4,'0d;" n)))))))

    ;; encode each character and write it to the stream
    (format stream "~{~a~}" (map 'list #'encode string))))

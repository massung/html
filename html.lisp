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
  (:use :cl :lexer :parse)
  (:export
   #:html
   #:html-parse
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

                  ;; name
                  tag

                  (loop
                     ;; tag attributes
                     for att in atts

                     ;; each attribute is a format
                     collect (destructuring-bind (k f &rest args)
                                 att
                               (list k (apply #'format nil f args))))

                  ;; the inner-text
                  body

                  ;; no close tag if a singleton tag
                  (find tag +singleton-tags+ :test #'string-equal)

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

;;; ----------------------------------------------------

(define-lexer html-lexer (s)

  ;; skip comments
  ("<!%-%-.-%-%->"     (values :next-token))

  ("<(%a%w*)"          (push-lexer s 'tag-lexer :tag $1))

  ;; close tag
  ("</(%a%w*)%s*>"     (values :close-tag $1))

  ;; CDATA sections
  ("<!%[CDATA%[.-%]%]" (values :cdata $$))

  ;; anything that isn't a tag is inner html
  (".[^<]*"            (values :inner-text $$)))

;;; ----------------------------------------------------

(define-lexer tag-lexer (s)
  (">"                 (pop-lexer s :end-tag))
  ("/>"                (pop-lexer s :singleton-tag))

  ;; skip whitespace
  ("[%s%n]+"           (values :next-token))

  ;; attributes
  ("%a%w*"             (values :att $$))
  ("="                 (values :eq))
  ("'(.-)'|\"(.-)\""   (values :value $1)))

;;; ----------------------------------------------------

(define-parser html-parser
  "HTML is just a list of tags."
  (.many1 'tag-parser))

;;; ----------------------------------------------------

(define-parser tag-parser
  "Tag name, attributes, inner-text, and close tag."
  (.let* ((tag (.is :tag))
          (atts (.many 'attribute-parser)))
    (if (find tag +singleton-tags+ :test #'string-equal)
        (>> (.one-of (.is :singleton-tag)
                     (.is :end-tag))
            (.ret (list tag atts)))
      (.one-of (>> (.is :singleton-tag) (.ret (list tag atts)))
               (>> (.is :end-tag)
                   (.let (inner-forms 'inner-html-parser)
                     (.ret (append (list tag atts) inner-forms))))))))

;;; ----------------------------------------------------

(define-parser attribute-parser
  "Parse all the attributes in a tag."
  (.let* ((att (.is :att))
          (eq (.is :eq))
          (value (.is :value)))
    (.ret (list att value))))

;;; ----------------------------------------------------

(define-parser inner-html-parser
  "Parse the text and tags inside another tag."
  (.many-until (.one-of 'tag-parser (.is :inner-text) (.is :cdata))
               (.is :close-tag)))

;;; ----------------------------------------------------

(defun html-parse (string &optional source)
  "Parse an HTML string and generate a Lisp form from it."
  (with-lexer (lexer 'html-lexer string :source source)
    (with-token-reader (next-token lexer)
      (parse 'html-parser next-token))))

;;; ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((dispatch-html (s c n)
           (declare (ignorable c n))
           (html-parse (with-output-to-string (html)
                         (do ((c (read-char s t nil t)
                                 (read-char s t nil t)))
                             ((and (char= c #\})
                                   (let ((x (peek-char nil s)))
                                     (char= x #\#))))
                           (princ c html))))))
    (set-dispatch-macro-character #\# #\{ #'dispatch-html)))

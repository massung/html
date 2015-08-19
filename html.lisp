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

(defconstant +tag-format+ "<~a~:{ ~a~@[='~:/html:html-format/'~]~}>~{~/html:html-format/~}~:[</~a>~;~]"
  "Format for rendering a tag string.")

;;; ----------------------------------------------------

(defconstant +singleton-tags+ '(:!doctype :area :base :br :col :command :embed :hr :img :input :link :meta :param :source)
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

        ;; CDATA sections render special
        (if (eq tag :cdata)
            (format stream "<![CDATA[~{~a~}]]>" body)

          ;; only encode if already encoding and not a language tag
          (let ((*encode-html-p* (and *encode-html-p*
                                      (not colonp)
                                      (not (find tag +lang-tags+)))))
            (format stream
                    +tag-format+

                    ;; name
                    tag

                    (loop
                       ;; tag attributes
                       for att in atts

                       ;; each attribute is a format
                       collect (destructuring-bind (k &optional f &rest args)
                                   att
                                 (list k (if (null args)
                                             f
                                           (apply #'format nil f args)))))

                    ;; the inner-text
                    body

                    ;; no close tag if a singleton tag
                    (find tag +singleton-tags+ :test #'string-equal)

                    ;; close tag
                    tag))))

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
  ("<!%-%-.-%-%->"           (values :next-token))

  ;; doctype
  ("<!DOCTYPE[%s%n]+(%a%w*)" (push-lexer s 'doctype-lexer :doctype $1))

  ;; whitespace
  ("[%s%n]+"                 (values :whitespace " "))

  ;; open tags
  ("<(%a%w*)"                (push-lexer s 'tag-lexer :tag $1))

  ;; close tag
  ("</(%a%w*)%s*>"           (values :close-tag $1))

  ;; CDATA sections
  ("<!%[CDATA%[(.-)%]%]"     (values :cdata $1))

  ;; character references
  ("&#x(%x+);"               (let ((n (parse-integer $1 :radix 16)))
                               (values :char (code-char n))))
  ("&#(%d+);"                (let ((n (parse-integer $1 :radix 10)))
                               (values :char (code-char n))))

  ;; entity references
  ("&(.-);"                  (values :char (html-entity $1)))

  ;; anything that isn't a tag is inner html
  (".[^%s%n&<]*"             (values :inner-text $$)))

;;; ----------------------------------------------------

(define-lexer doctype-lexer (s)
  (">"                       (pop-lexer s :end-doctype))

  ;; skip whitespace
  ("[%s%n]+"                 (values :next-token))

  ;; SYSTEM and PUBLIC external references
  ("SYSTEM[%s%n]+"           (values :system))
  ("PUBLIC[%s%n]+"           (values :public))

  ;; external reference links
  ("'(.-)'|\"(.-)\""         (values :ref $1)))

;;; ----------------------------------------------------

(define-lexer tag-lexer (s)
  (">"                       (pop-lexer s :end-tag))
  ("/>"                      (pop-lexer s :singleton-tag))

  ;; skip whitespace
  ("[%s%n]+"                 (values :next-token))

  ;; attributes
  ("(%a%w*)"                 (values :att $1))
  ("="                       (values :eq))
  ("'(.-)'|\"(.-)\""         (values :value $1)))

;;; ----------------------------------------------------

(define-parser html-parser
  "HTML is just a list of tags."
  (>> (.skip (.is :whitespace))

      ;; read a single element
      (.one-of 'doctype-parser
               'tag-parser
               'cdata-parser

               ;; just allow for random text?
               (.is :inner-text))))

;;; ----------------------------------------------------

(define-parser doctype-parser
  "Read the DOCTYPE and then parse a tag."
  (.let (root-tag (.is :doctype))
    (>> (.many-until (.any) (.is :end-doctype))

        ;; read a tag after the declaration
        (.let (tag (>> (.skip (.is :whitespace)) 'tag-parser))
          (.ret `(:!doctype ((,root-tag)) ,tag))))))

;;; ----------------------------------------------------

(define-parser tag-parser
  "Tag name, attributes, inner-text, and close tag."
  (.let* ((tag (.is :tag)) (atts (.many 'attribute-parser)))
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
  (.let (att (.is :att))
    (.opt (list att)
          (.let (value (>> (.is :eq) (.is :value)))
            (.ret (list att value))))))

;;; ----------------------------------------------------

(define-parser inner-html-parser
  "Parse the text and tags inside another tag."
  (.let (forms (.many (.one-of 'tag-parser
                               'cdata-parser

                               ;; whitespace coalesces
                               (.is :whitespace)
                               (.is :char)
                               (.is :inner-text))))

    ;; in HTML most close tags are optional, so ignore them
    (>> (.maybe (.is :close-tag)) (.ret forms))))

;;; ----------------------------------------------------

(define-parser cdata-parser
  "Parse a CDATA section."
  (.let (data (.is :cdata))
    (.ret (list :cdata nil data))))

;;; ----------------------------------------------------

(defun html-parse (string &optional source)
  "Parse an HTML string and generate a Lisp form from it."
  (with-lexer (lexer 'html-lexer string :source source)
    (with-token-reader (next-token lexer)
      (parse 'html-parser next-token))))

;;; ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((read-html (s c)
           (declare (ignore c))
           (let ((html (html-parse (with-output-to-string (html)
                                     (do ((c (read-char s t nil t)
                                             (read-char s t nil t)))
                                         ((char= c #\}))
                                       (princ c html))))))
             (list 'quote html))))
    (set-macro-character #\{ #'read-html)))

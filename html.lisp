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
                       collect (destructuring-bind (k &optional f &rest xs)
                                   att
                                 (list k (if (null xs)
                                             f
                                           (apply #'format nil f xs)))))

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

  ;; skip whitespace and comments
  ("[%s%n]+|<!%-%-.-%-%->" :next-token)

  ;; declaration tags
  ("<!(%a+)[%s%n]+([%a_:][%w:.-]*)"
   (if (string-equal $1 :doctype)
       (push-lexer s 'doctype-lexer :doctype $2)
     (error "Unrecognized HTML declaration tag ~s" $1)))

  ;; open tag
  ("<([%a_:][%w:.-]*)" (swap-lexer s 'attribute-lexer :tag $1)))

;;; ----------------------------------------------------

(define-lexer doctype-lexer (s)

  ;; end of declaration
  (">" (pop-lexer s :end-doctype))

  ;; skip whitespace, named tokens, and quoted values
  ("[%s%n]+|[%a_:][%w:.-]*|'.-'|\".-\"" :next-token))

;;; ----------------------------------------------------

(define-lexer attribute-lexer (s)

  ;; skip whitespace
  ("[%s%n]+" :next-token)

  ;; end of tag tokens
  ("/>" (pop-lexer s :singleton-tag))
  (">" (swap-lexer s 'tag-lexer :end-tag))

  ;; attribute key
  ("([%a_:][%w:.-]*)" (values :att $1))

  ;; quoted attribute values
  ("=[%s%n]*(?'(.-)'|\"(.-)\")" (values :value $1))

  ;; unquoted attribute values
  ("=[%s%n]*([^%s%n`'\"=<>]+)" (values :value $1)))

;;; ----------------------------------------------------

(define-lexer tag-lexer (s)

  ;; coalesce whitespace
  ("[%s%n]+" (values :whitespace #\space))

  ;; skip comments
  ("<!%-%-.-%-%->" :next-token)

  ;; close tag
  ("</([%a_:][%w:.-]*)%s*>" (pop-lexer s :close-tag $1))

  ;; CDATA sections
  ("<!%[CDATA%[(.-)%]%]" (values :cdata $1))

  ;; child tag
  ("<([%a_:][%w:.-]*)" (push-lexer s 'attribute-lexer :tag $1))

  ;; character references
  ("&#x(%x+);" (values :char (character-ref $1 :radix 16)))
  ("&#(%d+);" (values :char (character-ref $1 :radix 10)))
  ("&(.-);" (values :char (entity-ref $1)))

  ;; anything that isn't a tag or reference is inner html
  (".[^%s%n&<]*" (values :inner-text $$)))

;;; ----------------------------------------------------

(define-parser html-parser
  "HTML is just a list of tags."
  (.one-of 'doctype-parser 'tag-parser))

;;; ----------------------------------------------------

(define-parser doctype-parser
  "Read the DOCTYPE and then parse a tag."
  (.let (root (.is :doctype))
    (>> (.is :end-doctype)
        (.let (tag (>> (.skip (.is :whitespace)) 'tag-parser))
          (.ret `(:!doctype ((,root)) ,tag))))))

;;; ----------------------------------------------------

(define-parser tag-parser
  "Tag name, attributes, inner-text, and close tag."
  (.let* ((tag (.is :tag)) (atts (.many 'attribute-parser)))

    ;; singleton tags have no inner content
    (if (find tag +singleton-tags+ :test #'string-equal)
        (>> (.one-of (.is :singleton-tag)
                     (.is :end-tag))
            (.ret (list tag atts)))

      ;; check for hard-coded singleton tag or parse content
      (.one-of (>> (.is :singleton-tag) (.ret (list tag atts)))
               (>> (.is :end-tag)
                   (.let (inner-forms 'inner-html-parser)
                     (.ret (append (list tag atts) inner-forms))))))))

;;; ----------------------------------------------------

(define-parser attribute-parser
  "Parse all the attributes in a tag."
  (.let (att (.is :att))
    (.opt (list att t)

          ;; html attribute value are optional
          (.let (value (.is :value))
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

(defun html-parse (string &optional source req)
  "Parse an HTML string and generate a Lisp form from it."
  (with-lexer (lexer 'html-lexer string :source source)
    (with-token-reader (next-token lexer)
      (parse 'html-parser next-token :initial-state req))))

;;; ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((read-html (s c)
           (declare (ignore c))
           (let ((html (with-output-to-string (html)
                         (flet ((read-quote (quo)
                                  (do ((c (read-char s t nil t)
                                          (read-char s t nil t)))
                                      ((char= c quo)
                                       (princ c html))
                                    (princ c html))))

                           ;; read until the terminal for tags
                           (do ((c (read-char s t nil t)
                                   (read-char s t nil t)))
                               ((char= c #\}))
                             (princ c html)
                             (when (or (char= c #\")
                                       (char= c #\'))
                               (read-quote c)))))))
             (list 'quote (html-parse html)))))
    (set-macro-character #\{ #'read-html)))

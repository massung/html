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
  (:use :cl :lexer :parse :markup)
  (:export
   #:html
   #:html-parse))

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

(defconstant +opt-singleton-tags+ '(:body :colgroup :dd :dt :head :html :li :optgroup :option :p :tbody :td :tfoot :th :thead :tr)
  "Tags that are not strictly required to be closed.")

;;; ----------------------------------------------------

(defconstant +lang-tags+ '(:style :script)
  "Tags that do not encode their inner HTML text.")

;;; ----------------------------------------------------

(defun singleton-tag-p (tag)
  "T if tag is considered a singleton tag."
  (find tag +singleton-tags+ :test #'string-equal))

;;; ----------------------------------------------------

(defun opt-singleton-tag-p (tag)
  "T if tag is allowed to not have a matching close tag."
  (find tag +opt-singleton-tags+ :test #'string-equal))

;;; ----------------------------------------------------

(defun lang-tag-p (tag)
  "T if tag is a language tag."
  (find tag +lang-tags+ :test #'string-equal))

;;; ----------------------------------------------------

(defun html (form &optional stream)
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
          (let ((*encode-html-p* (when *encode-html-p*
                                   (not (or colonp (lang-tag-p tag))))))
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
                    (singleton-tag-p tag)

                    ;; close tag
                    tag))))

    ;; not a tag form, so just print it to the stream
    (if (or *encode-html-p* colonp)
        (write-string (markup-encode (princ-to-string form)) stream)
      (princ form stream))))

;;; ----------------------------------------------------

(defvar *tag-stack* nil
  "Stack of the tags currently being tokenized.")

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
  ("<([%a_:][%w:.-]*)"
   (multiple-value-prog1
       (push-lexer s 'attribute-lexer :tag $1)
     (push $1 *tag-stack*))))

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

  ;; singleton tag
  ("/>" (pop-lexer s :singleton-tag))

  ;; end of attributes, beginning of inner text
  (">" (if (lang-tag-p (first *tag-stack*))
           (swap-lexer s 'lang-lexer :end-tag)
         (swap-lexer s 'tag-lexer :end-tag)))

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
  ("<([%a_:][%w:.-]*)"
   (multiple-value-prog1
       (push-lexer s 'attribute-lexer :tag $1)
     (push $1 *tag-stack*)))

  ;; character references
  ("&#x(%x+);" (values :char (character-ref $1 :radix 16)))
  ("&#(%d+);" (values :char (character-ref $1 :radix 10)))
  ("&(.-);" (values :char (entity-ref $1)))

  ;; anything that isn't a tag or reference is inner html
  (".[^%s%n&<]*" (values :inner-text $$)))

;;; ----------------------------------------------------

(define-lexer lang-lexer (s)

  ;; close tag
  ("</([%a_:][%w:.-]*)%s*>" (pop-lexer s :close-tag $1))

  ;; quoted strings
  ("'.-'|\".-\"" (values :inner-text $$))

  ;; all text up to the close tag or quoted string
  (".[^'\"<]*" (values :inner-text $$)))

;;; ----------------------------------------------------

(define-parser html-parser
  "HTML is just a list of tags."
  (.either 'doctype-parser 'tag-parser))

;;; ----------------------------------------------------

(define-parser doctype-parser
  "Read the DOCTYPE and then parse a tag."
  (.let (root (.is :doctype))
    (.do (.is :end-doctype)
         (.let (tag (.do (.skip-many (.is :whitespace)) 'tag-parser))
           (.ret `(:!doctype ((,root)) ,tag))))))

;;; ----------------------------------------------------

(define-parser tag-parser
  "Tag name, attributes, inner-text, and close tag."
  (.let* ((tag (.is :tag)) (atts (.many 'attribute-parser)))

    ;; singleton tags have no inner content
    (if (singleton-tag-p tag)
        (.do (.or (.is :singleton-tag)
                  (.is :end-tag))
             (.ret (list tag atts)))

      ;; check for hard-coded singleton tag or parse content
      (.or (.do (.is :singleton-tag) (.ret (list tag atts)))
           (.do (.is :end-tag)
                (.push tag)
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
  (.let (forms (.many (.or 'tag-parser
                           'cdata-parser

                           ;; whitespace coalesces
                           (.is :whitespace)
                           (.is :char)
                           (.is :inner-text))))

    ;; ensure that the close tag matches
    (.let (top (.pop))
      (.or (.let (tag (.is :close-tag))
             (if (string-equal tag top)
                 (.ret forms)
               (.fail "Tag mismatch; expected ~s got ~s" top tag)))
           (.fail "Missing close tag for ~s" top)))))

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
      (let ((*tag-stack* nil))
        (parse 'html-parser next-token :initial-state req)))))


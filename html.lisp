;;;; HTML Rendering for Common Lisp
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
  (:use :cl :markup)
  (:export
   #:html-render
   #:html-format

   ;; macros
   #:define-html-tag))

(in-package :html)

;;; ----------------------------------------------------

(defun html-render (form)
  "Renders HTML to a string."
  (format nil "~/html:html-format/" form))

;;; ----------------------------------------------------

(defconstant +tag-format+
  "<~a~:{ ~a~@[='~@/html:html-format/'~]~}>~{~/html:html-format/~}~@[</~a>~]"
  "Format for rendering a tag string.")

;;; ----------------------------------------------------

(defconstant +singleton-tags+
  '(area base br col command embed hr img input link meta param source)
  "Tags that do not have matching close tags.")

;;; ----------------------------------------------------

(defconstant +language-tags+ '(script style)
  "Tags that do not HTML encode their contents.")

;;; ----------------------------------------------------

(defvar *encode-html-p* t
  "Contents of a tag should be encoded.")

;;; ----------------------------------------------------

(defun html-format (stream form &optional colonp atp &rest args)
  "Output an form to a stream as HTML."
  (declare (ignore colonp args))
  (if (listp form)
      (if (eq (first form) :cdata)
          (format stream "<![CDATA[~{~a~}]]>" (rest form))
        (apply 'html-format-tag stream form))
    (if (and (null atp) (null *encode-html-p*))
        (princ form stream)
      (write-string (markup-encode (princ-to-string form)) stream))))

;;; ----------------------------------------------------

(defun html-format-tag (stream tag &optional atts &rest content)
  "Output a tag form to a stream."
  (let* ((language-tag-p (find tag +language-tags+ :test 'string-equal))
         (singleton-tag-p (find tag +singleton-tags+ :test 'string-equal))

         ;; disable HTML-encoding of content in a language tag
         (*encode-html-p* (not language-tag-p)))
    (format stream +tag-format+
            tag
            atts
            content

            ;; singleton tags do not close their open tag
            (unless singleton-tag-p tag))))

;;; ----------------------------------------------------

(defmacro define-html-tag (name)
  "Create a function that will generate a tag form to render."
  (let ((f (intern (concatenate 'string "<" (string name) ">") *package*)))
    `(let ((symbol (defun ,f (&rest args)
                     (loop
                        for arg = (pop args)
                        while arg

                        ;; if a keyword, it's an attribute
                        when (keywordp arg)
                        collect (list arg (pop args))
                        into atts

                        ;; otherwise it's some content
                        unless (keywordp arg)
                        collect arg
                        into content

                        ;; construct the list to be rendered
                        finally (return `(,',name ,atts ,@content))))))
       (prog1 symbol
         (export symbol *package*)))))

;;; ----------------------------------------------------

(define-html-tag a)
(define-html-tag abbr)
(define-html-tag address)
(define-html-tag area)
(define-html-tag article)
(define-html-tag aside)
(define-html-tag audio)
(define-html-tag b)
(define-html-tag base)
(define-html-tag bdi)
(define-html-tag bdo)
(define-html-tag blockquote)
(define-html-tag body)
(define-html-tag br)
(define-html-tag button)
(define-html-tag canvas)
(define-html-tag caption)
(define-html-tag cite)
(define-html-tag code)
(define-html-tag col)
(define-html-tag colgroup)
(define-html-tag command)
(define-html-tag datalist)
(define-html-tag dd)
(define-html-tag del)
(define-html-tag details)
(define-html-tag dfn)
(define-html-tag div)
(define-html-tag dl)
(define-html-tag dt)
(define-html-tag em)
(define-html-tag embed)
(define-html-tag fieldset)
(define-html-tag figcaption)
(define-html-tag figure)
(define-html-tag footer)
(define-html-tag form)
(define-html-tag h1)
(define-html-tag h2)
(define-html-tag h3)
(define-html-tag h4)
(define-html-tag h5)
(define-html-tag h6)
(define-html-tag head)
(define-html-tag header)
(define-html-tag hgroup)
(define-html-tag hr)
(define-html-tag html)
(define-html-tag i)
(define-html-tag iframe)
(define-html-tag img)
(define-html-tag input)
(define-html-tag ins)
(define-html-tag kbd)
(define-html-tag keygen)
(define-html-tag label)
(define-html-tag legend)
(define-html-tag li)
(define-html-tag link)
(define-html-tag map)
(define-html-tag mark)
(define-html-tag menu)
(define-html-tag meta)
(define-html-tag meter)
(define-html-tag nav)
(define-html-tag noscript)
(define-html-tag object)
(define-html-tag ol)
(define-html-tag optgroup)
(define-html-tag option)
(define-html-tag output)
(define-html-tag p)
(define-html-tag param)
(define-html-tag pre)
(define-html-tag progress)
(define-html-tag q)
(define-html-tag rp)
(define-html-tag rt)
(define-html-tag ruby)
(define-html-tag s)
(define-html-tag samp)
(define-html-tag script)
(define-html-tag section)
(define-html-tag select)
(define-html-tag small)
(define-html-tag source)
(define-html-tag span)
(define-html-tag strong)
(define-html-tag style)
(define-html-tag sub)
(define-html-tag summary)
(define-html-tag sup)
(define-html-tag table)
(define-html-tag tbody)
(define-html-tag td)
(define-html-tag textarea)
(define-html-tag tfoot)
(define-html-tag th)
(define-html-tag thead)
(define-html-tag time)
(define-html-tag title)
(define-html-tag tr)
(define-html-tag track)
(define-html-tag u)
(define-html-tag ul)
(define-html-tag var)
(define-html-tag video)
(define-html-tag wbr)

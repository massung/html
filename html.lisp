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

   ;; doctype and cdata tags
   #:<!doctype>
   #:<!cdata>

   ;; tag accessors
   #:html-tag-name
   #:html-tag-attributes
   #:html-tag-elements

   ;; doctype accessors
   #:html-doctype-root

   ;; cdata accessors
   #:html-cdata-data

   ;; macros
   #:define-html-tag))

(in-package :html)

;;; ----------------------------------------------------

(defclass html-tag ()
  ((name :initarg :name :accessor html-tag-name)
   (atts :initarg :attributes :accessor html-tag-attributes)
   (elts :initarg :elements :accessor html-tag-elements))
  (:documentation "A basic HTML tag to be rendered."))

;;; ----------------------------------------------------

(defclass html-doctype ()
  ((root :initarg :root :accessor html-doctype-root))
  (:documentation "An HTML <!DOCTYPE> tag."))

;;; ----------------------------------------------------

(defclass html-cdata ()
  ((data :initarg :data :accessor html-cdata-data :initform nil))
  (:documentation "An HTML <![CDATA[..]]> inner HTML tag."))

;;; ----------------------------------------------------

(defmethod print-object ((tag html-tag) stream)
  "Output a tag to a stream."
  (print-unreadable-object (tag stream :type t)
    (prin1 (html-tag-name tag) stream)))

;;; ----------------------------------------------------

(defmethod print-object ((tag html-doctype) stream)
  "Output a doctype declaration to a stream."
  (print-unreadable-object (tag stream :type t)
    (prin1 (html-doctype-root tag) stream)))

;;; ----------------------------------------------------

(defconstant +singleton-tags+
  '(area base br col command embed hr img input link meta param source)
  "Tags that do not have matching close tags.")

;;; ----------------------------------------------------

(defconstant +language-tags+ '(script style)
  "Tags that do not HTML encode their contents.")

;;; ----------------------------------------------------

(defparameter *encode-html-p* t
  "T if non-tag forms should be encoded.")

;;; ----------------------------------------------------

(defun html-render (form &optional stream)
  "Render forms to an HTML string or output stream."
  (if stream
      (html-format stream form)
    (with-output-to-string (s)
      (html-format s form))))

;;; ----------------------------------------------------

(defmethod html-format (stream (form t) &optional colonp &rest xs)
  "Output an form to a stream as HTML."
  (declare (ignore xs))
  (html-format stream (princ-to-string form) colonp))

;;; ----------------------------------------------------

(defmethod html-format (stream (s string) &optional colonp &rest xs)
  "Output a string to a stream."
  (declare (ignore xs))
  (if (or colonp *encode-html-p*)
      (write-string (markup-encode s) stream)
    (princ s stream)))

;;; ----------------------------------------------------

(defmethod html-format (stream (seq sequence) &optional colonp &rest xs)
  "Output a sequence of forms to a stream."
  (declare (ignore xs))
  (map nil #'(lambda (i) (html-format stream i colonp)) seq))

;;; ----------------------------------------------------

(defmethod html-format (stream (tag html-doctype) &optional colonp &rest xs)
  "Output a DOCTYPE declaration to a stream."
  (declare (ignore colonp xs))
  (format stream "~@[<!DOCTYPE ~a>~]" (html-doctype-root tag)))

;;; ----------------------------------------------------

(defmethod html-format (stream (tag html-cdata) &optional colonp &rest xs)
  "Output CDATA to a stream."
  (declare (ignore colonp xs))
  (format stream "~@[<![CDATA[~a]]>~]" (html-cdata-data tag)))

;;; ----------------------------------------------------

(defmethod html-format (stream (tag html-tag) &optional colonp &rest xs)
  "Ouput an HTML tag to a stream."
  (declare (ignore xs))
  (with-slots (name atts elts)
      tag

    ;; is this a language or singleton tag?
    (let ((language-p (find name +language-tags+ :test 'string-equal))
          (singleton-p (find name +singleton-tags+ :test 'string-equal)))

      ;; write the tag
      (write-char #\< stream)
      (write-string name stream)

      ;; write out all the attribute elements
      (format stream "~:{ ~a~@[='~:/html:html-format/'~]~}" atts)

      ;; close empty, non-singleton tags
      (unless (or elts singleton-p)
        (write-char #\/ stream))
      (write-char #\> stream)

      ;; write all the elements
      (unless (or (null elts) singleton-p)
        (let ((*encode-html-p* (or colonp (not language-p))))
          (dolist (form elts)
            (html-format stream form)))

        ;; output the close tag
        (format stream "</~a>" name)))))

;;; ----------------------------------------------------

(defun <!doctype> (&optional root)
  "Create a simple DOCTYPE declaration tag."
  (make-instance 'html-doctype :root root))

;;; ----------------------------------------------------

(defun <!cdata> (&optional data)
  "Create a CDATA tag."
  (make-instance 'html-cdata :data data))

;;; ----------------------------------------------------

(defmacro define-html-tag (name)
  "Create a function that will generate a tag form to render."
  (let ((f (intern (concatenate 'string "<" (string name) ">") *package*)))
    `(let ((symbol (defun ,f (&rest args)
                     (loop
                        for arg = (pop args)
                        while arg

                        ;; keywords are attributes
                        when (keywordp arg)
                        collect (list arg (pop args))
                        into atts

                        ;; otherwise it's an element
                        unless (keywordp arg)
                        collect arg
                        into xs

                        ;; construct the tag
                        finally (return (make-instance 'html-tag
                                                       :name ,(string name)
                                                       :attributes atts
                                                       :elements xs))))))
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

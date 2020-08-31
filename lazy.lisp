;;;; Lazy forms in Common Lisp
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

(defpackage :lazy
  (:use :cl)
  (:export
   #:lazy
   #:lazy-value))

(in-package :lazy)

;;; ----------------------------------------------------

(defclass thunk ()
  ((form  :initarg :form)
   (values :initarg :values))
  (:documentation "A form to be evaluated later."))

;;; ----------------------------------------------------

(defmethod print-object ((obj thunk) stream)
  "Output a lazy form."
  (print-unreadable-object (obj stream :type t)
    (if (slot-boundp obj 'values)
        (prin1 (lazy-value obj) stream)
      (princ "UNREALIZED" stream))))

;;; ----------------------------------------------------

(defmethod thunk-realized-p ((thunk thunk))
  "T if the thunk has been realized to a value."
  (slot-boundp thunk 'values))

;;; ----------------------------------------------------

(defmacro lazy (&body form)
  "Create a new lazy form that will evaluate later."
  `(make-instance 'thunk :form (lambda () ,@form)))

;;; ----------------------------------------------------

(defun lazy-value (thunk)
  "Get the value of a lazy form."
  (with-slots (form values)
      thunk
    (unless (thunk-realized-p thunk)
      (setf values (multiple-value-list
                    (funcall form))))
    (values-list values)))

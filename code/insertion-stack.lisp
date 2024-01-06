;;;; insertion-stack.lisp --- A stack of possible insertion entries.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; `insertion-entry'

(defclass insertion-entry (pi:print-items-mixin)
  ((%backward :initarg  :backward
              :accessor backward
              :initform '())
   (%forward  :initarg  :forward
              :accessor forward
              :initform '())))

(defmethod clone ((object insertion-entry) &key)
  (make-instance 'insertion-entry :backward (backward object)
                                  :forward  (forward object)))

(defmethod pi:print-items append ((object insertion-entry))
  (let ((before (coerce          (backward object)  'string))
        (after  (coerce (reverse (forward  object)) 'string)))
    `(((:before (:before :middle)) "~A" ,before)
      (:middle                     "|")
      ((:after  (:after  :middle)) "~A" ,after))))

(defmethod insertion ((entry insertion-entry))
  (concatenate 'string (backward entry) (reverse (forward entry))))

;;; `insertion-stack'
;;;
;;; A stack of `insertion-entry's.

(defclass insertion-stack ()
  ((%entries :initarg  :entries
             :reader   entries
             :initform (make-array 0 :fill-pointer 0 :adjustable t))))

(defmethod clone ((object insertion-stack) &key)
  (let* ((entries     (entries object))
         (new-entries (make-array (length entries) :fill-pointer t :adjustable t))
         (new-entries (map-into new-entries #'clone entries)))
    (make-instance 'insertion-stack :entries new-entries)))

(defmethod entry-count ((insertion-stack insertion-stack))
  (length (entries insertion-stack)))

(defmethod top-entry ((insertion-stack insertion-stack))
  (let* ((entries (entries insertion-stack))
         (length  (length entries)))
    (when (plusp length)
      (aref entries (1- length)))))

(defmethod find-entry ((index t) (insertion-stack insertion-stack))
  (aref (entries insertion-stack) index))

(defmethod push-entry ((insertion-stack insertion-stack))
  (let ((new-entry (make-instance 'insertion-entry)))
    (vector-push-extend new-entry (entries insertion-stack))
    new-entry))

(defmethod pop-entry ((insertion-stack insertion-stack))
  (let ((entries (entries insertion-stack)))
    (if (plusp (length entries))
        (vector-pop entries)
        (error 'insertion-stack-empty-error :site nil)))) ; TODO get the site somehow?

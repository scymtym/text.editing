;;;; protocol.lisp --- Protocol provided by the expression module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.expression)

;;; Expression node protocol

(defgeneric range (expression)
  (:documentation
   "Return the range, that is start and end position, of EXPRESSION."))

(defgeneric children (expression)
  (:documentation
   "Return the child expressions of EXPRESSION as a list."))

;;; Expression tree protocol

(defgeneric map-expressions-containing-cursor (function cursor syntax-tree
                                               &key start-relation
                                                    end-relation))

(defgeneric map-expressions-containing-cursor-using-buffer
    (function buffer cursor syntax-tree &key start-relation end-relation))

(defgeneric expressions-containing-cursor
    (cursor syntax-tree &key start-relation end-relation count))

(defun innermost-expression-containing-cursor
    (cursor syntax-tree &rest args &key start-relation end-relation)
  (declare (ignore start-relation end-relation))
  (flet ((capture (expression)
           (return-from innermost-expression-containing-cursor expression)))
    (declare (dynamic-extent #'capture))
    (apply #'map-expressions-containing-cursor #'capture cursor syntax-tree
           args)
    nil))

(defun outermost-expression-containing-cursor
    (cursor syntax-tree &rest args &key start-relation end-relation)
  (declare (ignore  start-relation end-relation))
  ;; Override the value of RESULT in successive calls since the
  ;; outermost expression corresponds to the final call of CAPTURE.
  (let ((result nil))
    (flet ((capture (expression)
             (setf result expression)))
      (declare (dynamic-extent #'capture))
      (apply #'map-expressions-containing-cursor #'capture cursor syntax-tree
             args))
    result))

;;; Default behavior

(defmethod map-expressions-containing-cursor
    ((function t) (cursor cluffer:cursor) (syntax-tree t)
     &rest args &key start-relation end-relation)
  (declare (ignore start-relation end-relation))
  (apply #'map-expressions-containing-cursor-using-buffer
         function (cluffer:buffer cursor) cursor syntax-tree
         args))

(defmethod expressions-containing-cursor
    ((cursor cluffer:cursor) (syntax-tree t)
     &rest args &key start-relation end-relation count)
  (declare (ignore start-relation end-relation))
  (let ((remaining count)
        (result    '()))
    (block nil
      (flet ((collect (expression)
               (push expression result)
               (cond ((null remaining))
                     ((zerop (decf remaining))
                      (return)))))
        (declare (dynamic-extent #'collect))
        (apply #'map-expressions-containing-cursor #'collect cursor syntax-tree
               (a:remove-from-plist args :count))))
    (nreverse result)))

;;; Expression unit protocol

(defgeneric syntax-tree (unit)
  (:documentation
   "Return a designator of the syntax tree UNIT should operate on."))

;;; Operations

(defgeneric raise (cursor unit direction))

(defgeneric splice (cursor unit direction))

(defgeneric split (cursor unit))

(defgeneric join (cursor unit))

;;;; direction.lisp --- Functions for handling the direction of operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Opposite direction

(declaim (inline opposite-direction))
(defun opposite-direction (direction)
  (ecase direction
    (:forward  :backward)
    (:backward :forward)))

;;; Opposite function pairs

(defun find-opposite (name &key (if-does-not-exist
                                 (lambda (name)
                                   (error "~@<No opposite is known for ~S.~@:>"
                                          name))))
  (cond ((get name 'opposite))
        ((typep if-does-not-exist '(or function (and symbol (not null))))
         (funcall if-does-not-exist name))
        (t
         if-does-not-exist)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mark-opposite-pair (name1 name2)
    (setf (get name1 'opposite) name2
          (get name2 'opposite) name1)))

(defmacro opposite-pair (name1 name2)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (mark-opposite-pair ',name1 ',name2)))

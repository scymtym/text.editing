;;;; items.lisp --- Function for getting and setting item sequences.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(defmethod map-items ((function  t)
                      (cursor    c:cursor)
                      (unit      t)
                      (direction (eql :forward)))
  (let ((function (a:ensure-function function)))
    (with-cloned-cursor (cursor cursor)
      (flet ((visit (cursor item)
               (funcall function item)
               (move-item-forward cursor)))
        (declare (dynamic-extent #'visit))
        (apply-from-cursor #'visit cursor unit direction)))))

(defmethod items ((cursor c:cursor) (unit t) (direction (eql :forward)))
  (let ((result (make-array 0 :fill-pointer 0 :adjustable t)))
    (flet ((collect (item)
             (vector-push-extend item result)))
      (declare (dynamic-extent #'collect))
      (map-items #'collect cursor unit direction))
    result))

(defmethod (setf items) ((new-value sequence)
                         (cursor    c:cursor)
                         (unit      t)
                         (direction t))
  (delete cursor unit direction)
  (insert-items cursor new-value)
  new-value)

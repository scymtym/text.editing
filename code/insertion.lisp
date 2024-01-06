;;;; insertion.lisp --- Operations for inserting items.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(declaim (inline %insert-item))
(defun %insert-item (cursor item)
  (if (eql item #\Newline)
      (c:split-line cursor)
      (c:insert-item cursor item)))

(defmethod insert-item ((cursor c:cursor) (item t))
  (%insert-item cursor item))

(defmethod insert-newline ((cursor c:cursor))
  (c:split-line cursor))

(defmethod insert-items ((cursor c:cursor) (items sequence)
                         &key (start 0) end )
  (loop :for i :from start :below (or end (length items))
        :for character = (elt items i)
        :do (%insert-item cursor character)))

(defmethod insert-items ((cursor c:cursor) (items list)
                         &key (start 0) end)

  (loop :with list = (nthcdr start items)
        :for i :from start
        :for character :in list
        :until (and end (= i end))
        :do (%insert-item cursor character)))

(defmethod insert-items ((cursor c:cursor) (items string)
                         &key (start 0) end)
  (loop :for i :from start :below (or end (length items))
        :for character = (aref items i)
        :do (%insert-item cursor character)))

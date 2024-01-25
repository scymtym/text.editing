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
                         &key (start 0) end)
  (loop :for i :from start :below (or end (length items))
        :for character = (elt items i)
        :do (%insert-item cursor character)))

(defmethod insert-items ((cursor cluffer-standard-line::left-sticky-cursor)
                         (items  sequence)
                         &key (start 0) end)
  (loop :for i :from (1- (or end (length items))) :downto start
        :for character = (elt items i)
        :do (%insert-item cursor character)))

(flet ((insert-list (cursor items start end)
         (loop :with list = (nthcdr start items)
               :for i :of-type alexandria:array-index :from start
               :for character :in list
               :until (and end (= i end))
               :do (%insert-item cursor character))))
  (declare (inline insert-list))

  (defmethod insert-items ((cursor c:cursor) (items list) &key (start 0) end)
    (insert-list cursor items start end))

  (defmethod insert-items ((cursor cluffer-standard-line::left-sticky-cursor)
                           (items  list)
                           &key (start 0) end)
    (let ((sub-sequence (subseq items start end)))
      (unless (null sub-sequence) ; null may change applicable methods
        (insert-list cursor (nreverse sub-sequence) 0 nil)))))

(defmethod insert-items ((cursor c:cursor) (items string) &key (start 0) end)
  (loop :for i :from start :below (or end (length items))
        :for character = (aref items i)
        :do (%insert-item cursor character)))

(defmethod insert-items ((cursor cluffer-standard-line::left-sticky-cursor)
                         (items  string)
                         &key (start 0) end)
  (loop :for i :from (1- (or end (length items))) :downto start
        :for character = (aref items i)
        :do (%insert-item cursor character)))

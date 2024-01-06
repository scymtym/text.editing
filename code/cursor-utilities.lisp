;;;; cursor-utilities.lisp --- Cursor-related utilities used in the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Item utilities

;;; CURSOR is an attached cursor
(defun item-after-cursor* (cursor)
  (if (c:end-of-line-p cursor)
      #\Newline
      (c:item-after-cursor cursor)))

(defun item-before-cursor* (cursor)
  (if (c:beginning-of-line-p cursor)
      #\Newline
      (c:item-before-cursor cursor)))

;;; Primitive cursor movement

(defun move-item-forward (cursor)
  (cond ((c:end-of-buffer-p cursor)
         (error 'c:end-of-buffer))
        ((c:end-of-line-p cursor)
         (move-cursor-to-line cursor (1+ (c:line-number cursor))))
        (t
         (c:forward-item cursor))))

(defun move-item-backward (cursor)
  (cond ((c:beginning-of-buffer-p cursor)
         (error 'c:beginning-of-buffer))
        ((c:beginning-of-line-p cursor)
         (move-cursor-to-line cursor (1- (c:line-number cursor)) :end))
        (t
         (c:backward-item cursor))))

;;; Primitive deletion

(defmethod delete-item-forward ((cursor c:cursor))
  (cond ((c:end-of-buffer-p cursor)
         (error 'c:end-of-buffer))
        ((c:end-of-line-p cursor)
         (c:join-line cursor))
        (t
         (c:delete-item cursor))))

(defmethod delete-item-backward ((cursor c:cursor))
  (cond ((c:beginning-of-buffer-p cursor)
         (error 'c:beginning-of-buffer))
        ((c:beginning-of-line-p cursor)
         (c:join-line (previous-line cursor)))
        (t
         (c:erase-item cursor))))

;;; Cursor attach helpers

(declaim (inline coerce-to-line coerce-to-position))
(defun coerce-to-line (line-designator &key buffer cursor)
  (check-type line-designator line-designator)
  (flet ((buffer ()
           (or buffer (c:buffer cursor))))
    (typecase line-designator
      ((eql :first)
       (c:find-line (buffer) 0))
      ((eql :last)
       (let ((buffer (buffer)))
         (c:find-line buffer (1- (c:line-count buffer)))))
      (integer
       (c:find-line (buffer) line-designator))
      (t
       line-designator))))

(defun coerce-to-position (position-designator line)
  (check-type position-designator position-designator)
  (case position-designator
    (:start 0)
    (:end   (c:item-count line))
    (t      position-designator)))

(defgeneric move-cursor-to-line (cursor line &optional position)
  (:method ((cursor c:cursor) (new-line c:line) &optional (position 0))
    (let ((current-line       (when (c:cursor-attached-p cursor)
                                (c:line cursor)))
          (effective-position (coerce-to-position position new-line)))
      ;; Signal the error before CURSOR gets detached from its current
      ;; line.
      (unless (<= 0 effective-position (cluffer:item-count new-line))
        (error 'cluffer:end-of-line))
      (cond ((null current-line)
             (c:attach-cursor cursor new-line effective-position))
            ((not (eq current-line new-line))
             (c:detach-cursor cursor)
             (c:attach-cursor cursor new-line effective-position))
            ((/= (c:cursor-position cursor) effective-position)
             (setf (c:cursor-position cursor) effective-position))))
    new-line)

  (:method ((cursor c:cursor) (new-line t) &optional (position 0))
    (let* ((buffer   (c:buffer cursor))
           (new-line (coerce-to-line new-line :buffer buffer :cursor cursor)))
      (move-cursor-to-line cursor new-line position))))

;;; Temporary cursor

(defun attach-cursor (cursor line &key buffer (position 0))
  (check-type line line-designator)
  (check-type position position-designator)
  (let* ((effective-line     (coerce-to-line line :buffer buffer))
         (effective-position (coerce-to-position position effective-line)))
    (c:attach-cursor cursor effective-line effective-position)
    cursor))

(defun call-with-attached-cursor (continuation cursor buffer line position)
  (attach-cursor cursor line :buffer buffer :position position)
  (unwind-protect
       (funcall continuation cursor)
    (c:detach-cursor cursor)))

(defmacro with-attached-cursor ((cursor-var buffer
                                 &rest args &key line position)
                                &body body)
  (declare (ignore line position))
  `(call-with-attached-cursor
    (lambda (,cursor-var) ,@body)
    ,buffer ,@args))

(defun call-with-temporary-cursor (continuation buffer
                                   &key (cursor-class 'cluffer-standard-line:right-sticky-cursor)
                                        (line         0)
                                        (position     0))
  (let ((cursor (make-instance cursor-class)))
    (call-with-attached-cursor continuation cursor buffer line position)))

(defmacro with-temporary-cursor ((cursor-var buffer
                                  &rest args &key cursor-class line position)
                                 &body body)
  (declare (ignore cursor-class line position))
  (check-type cursor-var symbol)
  `(call-with-temporary-cursor (lambda (,cursor-var) ,@body) ,buffer ,@args))

(defun call-with-cloned-cursor (continuation cursor
                                &key (buffer       (c:buffer cursor))
                                     (cursor-class nil cursor-class-supplied?)
                                     (line         (c:line cursor))
                                     (position     (c:cursor-position cursor)))
  (let ((clone (apply #'clone-cursor cursor (if cursor-class-supplied?
                                                (list :class cursor-class)
                                                '()))))
    (call-with-attached-cursor continuation clone buffer line position)))

(defmacro with-cloned-cursor ((cursor-var cursor
                               &rest args &key cursor-class line position)
                              &body body)
  (declare (ignore cursor-class line position))
  (check-type cursor-var symbol)
  `(call-with-cloned-cursor (lambda (,cursor-var) ,@body) ,cursor ,@args))

;;; Mapping and ranges

(defun map-to-cursor! (function start-cursor end-cursor)
  (loop :until (c:cursor= start-cursor end-cursor)
        :do (funcall function start-cursor)))

(defun map-to-cursor (function start-cursor end-cursor)
  (with-cloned-cursor (cursor start-cursor)
    (map-to-cursor! function cursor end-cursor)))

(defun map-range! (function start-cursor end-cursor)
  (map-to-cursor! (lambda (cursor)
                    (funcall function cursor)
                    (move-item-forward cursor))
                  start-cursor end-cursor))

(defun map-range (function start-cursor end-cursor)
  (map-to-cursor (lambda (cursor)
                   (funcall function cursor)
                   (move-item-forward cursor))
                 start-cursor end-cursor))

(defun apply-at-cursor-until (continuation cursor cursor-predicate
                              item-predicate item-key) ; TODO item-key, item-predicate optional
  (loop :with cursor-predicate = (a:ensure-function cursor-predicate)
        :with item-predicate = (when item-predicate
                                 (a:ensure-function item-predicate))
        :until (funcall cursor-predicate cursor)
        :do (let ((item (funcall item-key cursor)))
              (when (and item-predicate (funcall item-predicate item))
                (loop-finish))
              (funcall continuation cursor item))))

;;; Deleting ranges

(defun delete-range-forward (start-cursor end-cursor)
  (map-to-cursor! #'delete-item-forward start-cursor end-cursor))

(defun delete-range-backward (start-cursor end-cursor)
  (map-to-cursor! #'delete-item-backward start-cursor end-cursor))

(defun delete-range (start-cursor end-cursor)
  ;; TODO determine direction
  (map-to-cursor #'delete-item-forward start-cursor end-cursor))

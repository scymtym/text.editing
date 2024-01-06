;;;; deletion.lisp --- Operations for deleting items.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; `delete' methods for units and directions

(macrolet ((define (direction deleter)
             `(defmethod delete ((cursor    c:cursor)
                                 (unit      t)
                                 (direction (eql ,direction)))
                (flet ((delete-one (cursor item)
                         (declare (ignore item))
                         (,deleter cursor)))
                  (declare (dynamic-extent #'delete-one))
                  (apply-from-cursor #'delete-one cursor unit direction)))))
  (define :forward  delete-item-forward)
  (define :backward delete-item-backward))

(defmethod delete ((cursor    c:cursor)
                   (unit      semi-line)
                   (direction (eql :forward)))
  ;; At the end of the line. join the line of CURSOR and the next
  ;; line.
  (if (c:end-of-line-p cursor)
      (delete-item-forward cursor)
      (call-next-method)))

(defmethod delete ((cursor    c:cursor)
                   (unit      semi-line)
                   (direction (eql :backward)))
  ;; At the beginning of the line, join the previous line and the line
  ;; of CURSOR.
  (if (c:beginning-of-line-p cursor)
      (delete-item-backward cursor)
      (call-next-method)))

;;; Some of the prose units have complicated predicates that wouldn't
;;; work if predicate calls were interleaved with deleting items.
(macrolet ((define (direction deleter)
             `(defmethod delete ((cursor    c:cursor)
                                 (unit      prose-unit)
                                 (direction (eql ,direction)))
                (with-cloned-cursor (end-cursor cursor)
                  (move end-cursor unit direction)
                  (,deleter cursor end-cursor)))))
  (define :forward  delete-range-forward)
  (define :backward delete-range-backward))

;;; Specialized operations

(defun delete-whitespace-forward (cursor)
  (apply-at-cursor-until
   (lambda (cursor item)
     (declare (ignore item))
     (c:delete-item cursor))
   cursor #'c:end-of-line-p #'not-whitespace-p #'c:item-after-cursor))

(defun delete-whitespace-backward (cursor)
  (apply-at-cursor-until
   (lambda (cursor item)
     (declare (ignore item))
     (c:erase-item cursor))
   cursor #'c:beginning-of-line-p #'not-whitespace-p #'c:item-before-cursor))

(defmethod delete-indentation ((cursor c:cursor))
  ;; Join current and previous lines.
  (c:beginning-of-line cursor)
  (unless (c:beginning-of-buffer-p cursor)
    (c:join-line (previous-line cursor)))
  ;; Replace all whitespace on the joined line before and after the
  ;; cursor with a single space.
  (delete-whitespace-backward cursor)
  (delete-whitespace-forward cursor)
  (unless (or (c:beginning-of-line-p cursor) (c:end-of-line-p cursor))
    (c:insert-item cursor #\Space)
    (c:backward-item cursor)))

(defmethod delete-trailing-whitespace ((cursor c:cursor))
  (with-cloned-cursor (cursor cursor)
    (c:end-of-line cursor)
    (loop :do (delete-whitespace-backward cursor)
              (if (c:end-of-buffer-p cursor)
                  (loop-finish)
                  (move-line-forward cursor :end)))))

(defmethod fixup-whitespace ((cursor c:cursor))
  (delete-whitespace-backward cursor)
  (delete-whitespace-forward cursor)
  (unless (c:beginning-of-line-p cursor)
    (c:insert-item cursor #\Space)
    (c:backward-item cursor)))

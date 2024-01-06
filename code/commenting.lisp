;;;; commenting.lisp --- Operations for adding and removing comments.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Line comments

(defmethod comment ((cursor t) (unit line) (direction t)
                    &key (comment-syntax ";; "))
  (with-cloned-cursor (cursor cursor)
    (back-to-indentation cursor)
    (insert-items cursor comment-syntax)))

(defmethod uncomment ((cursor t) (unit line) (direction t))
  (with-cloned-cursor (cursor cursor)
    (back-to-indentation cursor)
    (apply-at-cursor-until
     (lambda (cursor item)
       (declare (ignore item))
       (delete-item-forward cursor))
     cursor #'c:end-of-line-p (a:curry #'char/= #\;) #'item-after-cursor*)))

;;; Other units

(defmethod comment ((cursor c:cursor) (unit t) (direction t)
                    &key (comment-syntax "#|"))
  (let ((open   comment-syntax)
        (close  (reverse comment-syntax))
        (first? t))
    (multiple-value-bind (first second mover)
        (ecase direction
          (:forward  (values open  close #'move-item-forward))
          (:backward (values close open  #'move-item-backward)))
      (apply-from-cursor (lambda (cursor item)
                           (declare (ignore item))
                           (when first?
                             (insert-items cursor first)
                             (setf first? nil))
                           (funcall mover cursor))
                         cursor unit direction)
      (insert-items cursor second))))

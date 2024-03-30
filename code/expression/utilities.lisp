;;;; utilities.lisp --- Utilities used by the expression nodule.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.expression)

(defun move-to-expression-start (cursor expression)
  (multiple-value-bind (start-line start-column) (range expression)
    (edit:move-cursor-to-line cursor start-line start-column)))

(defun move-to-expression-end (cursor expression)
  (multiple-value-bind (start-line start-column end-line end-column)
      (range expression)
    (declare (ignore start-line start-column))
    (edit:move-cursor-to-line cursor end-line end-column)))

(defmacro with-cloned-cursor-at-expression-start ((cursor-var cursor expression)
                                                  &body body)
  `(edit:with-cloned-cursor (,cursor-var ,cursor)
     (move-to-expression-start ,cursor-var ,expression)
     ,@body))

(defmacro with-cloned-cursor-at-expression-end ((cursor-var cursor expression)
                                                  &body body)
  `(edit:with-cloned-cursor (,cursor-var ,cursor)
     (move-to-expression-end ,cursor-var ,expression)
     ,@body))

(defun move-delimiter-forward (cursor &key (test (lambda (character)
                                                   (find character "#(\""))))
  (edit::apply-at-cursor-until
   (lambda (cursor item)
     (declare (ignore item))
     (edit:move-item-forward cursor))
   cursor #'cluffer:end-of-buffer-p
   (complement test) #'edit::item-after-cursor*))

(defun move-delimiter-backward (cursor &key (test (lambda (character)
                                                    (find character ")\""))))
  (edit::apply-at-cursor-until
   (lambda (cursor item)
     (declare (ignore item))
     (edit:move-item-backward cursor))
   cursor #'cluffer:beginning-of-buffer-p
   (complement test) #'edit::item-before-cursor*))

;;; Starting from the location of CURSOR, locate the next/previous
;;; expression and possibly move CURSOR to the beginning/end of that
;;; expressions.  If such an expression can be located, return it and
;;; its ancestors otherwise return `nil'.
(defun maybe-advance-to-expression
    (cursor direction syntax-tree
     &key (start-relation     '<)
          (end-relation       '<)
          (expressions        (expressions-containing-cursor
                               cursor syntax-tree
                               :start-relation start-relation
                               :end-relation   end-relation
                               :count          2))
          (initial-expression (first expressions))
          (advance-to-child   t)
          (move-cursor        t))
  (flet ((advance (cursor &key stop-line stop-column)
           (let ((result    nil)
                 (ancestors '()))
             (edit::apply-at-cursor-until
              (ecase direction
                (:forward  (lambda (cursor item)
                             (declare (ignore item))
                             (edit:move-item-forward cursor)))
                (:backward (lambda (cursor item)
                             (declare (ignore item))
                             (edit:move-item-backward cursor))))
              cursor
              (lambda (cursor)
                (or (ecase direction
                      (:forward  (cluffer:end-of-buffer-p cursor))
                      (:backward (cluffer:beginning-of-buffer-p cursor)))
                    (and stop-line stop-column
                         (= (cluffer:cursor-position cursor) stop-column)
                         (= (cluffer:line-number     cursor) stop-line))
                    (let ((expressions (expressions-containing-cursor
                                        cursor syntax-tree
                                        :start-relation start-relation
                                        :end-relation   end-relation
                                        :count          2)))
                      (setf ancestors (rest expressions))
                      (and expressions
                           (not (eq (setf result (first expressions))
                                    initial-expression)))))))
             (values result ancestors))))
    (cond (initial-expression
           (multiple-value-bind (start-line start-column end-line end-column)
               (range initial-expression)
             (if (or (not advance-to-child)
                     (null (children initial-expression))
                     (ecase direction
                       (:forward  (and (= start-column (cluffer:cursor-position cursor))
                                       (= start-line   (cluffer:line-number cursor))))
                       (:backward (and (= end-column   (cluffer:cursor-position cursor))
                                       (= end-line     (cluffer:line-number cursor))))))
                 ;; If not requested to advance to next/previous child
                 ;; or there are no children or CURSOR is on the
                 ;; boundary of the initial expression (instead of the
                 ;; interior), use the initial expression
                 (values initial-expression (rest expressions))
                 ;; If requested to and able to, advance to
                 ;; next/previous child.
                 (edit:with-cloned-cursor (scan cursor)
                   ;; EXPRESSION is never null since, in the worst
                   ;; case, the SCAN cursor ends up on the boundary of
                   ;; INITIAL-EXPRESSION which is still an expression.
                   (multiple-value-bind (expression ancestors)
                       (ecase direction
                         (:forward  (advance scan :stop-line   end-line
                                                  :stop-column end-column))
                         (:backward (advance scan :stop-line   start-line
                                                  :stop-column start-column)))
                     (when move-cursor
                       (edit:move-cursor-to-line
                        cursor (cluffer:line scan) (cluffer:cursor-position scan)))
                     (values expression ancestors))))))
          ;; No initial expression. Advance to next/previous
          ;; expression.
          (move-cursor
           (advance cursor))
          (t
           (edit:with-cloned-cursor (scan cursor)
             (advance scan))))))

;;;; utilities.lisp --- Utilities used by the expression nodule.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.expression)

(defun expression<= (expression1 expression2)
  (multiple-value-bind (start-line1 start-column1 end-line1 end-column1)
      (range expression1)
    (declare (ignore start-line1 start-column1))
    (multiple-value-bind (start-line2 start-column2) (range expression2)
      (or (< end-line1 start-line2)
          (and (= end-line1 start-line2)
               (<= end-column1 start-column2))))))

;;; TODO replace this with a protocol function for querying properties
;;; of the expression.
(defun compound-expression-p (expression buffer)
  (multiple-value-bind (start-line start-column end-line end-column)
      (range expression)
    (declare (ignore start-line start-column))
    (let* ((end-line  (cluffer:find-line buffer end-line))
           (last-item (cluffer:item-at-position end-line (1- end-column))))
      (eql last-item #\)))))

(defun move-to-expression-boundary (cursor expression which)
  (multiple-value-bind (line column)
      (ecase which
        ((:start :backward) (range expression))
        ((:end   :forward)  (multiple-value-bind (start-line start-column
                                                  end-line   end-column)
                                (range expression)
                              (declare (ignore start-line start-column))
                              (values end-line end-column))))
    (edit:move-cursor-to-line cursor line column)))

(defmacro with-cursor-at-expression-boundary
    ((cursor-var cursor expression which) &body body)
  `(edit:with-cloned-cursor (,cursor-var ,cursor)
     (move-to-expression-boundary ,cursor-var ,expression ,which)
     ,@body))

(defgeneric move-delimiter (cursor direction &key test)
  (:method ((cursor c:cursor) (direction (eql :forward))
            &key (test (lambda (character)
                         (find character "#(\""))))
    (edit::apply-at-cursor-until
     (lambda (cursor item)
       (declare (ignore item))
       (edit:move-item-forward cursor))
     cursor #'cluffer:end-of-buffer-p
     (complement test) #'edit::item-after-cursor*))

  (:method ((cursor c:cursor) (direction (eql :backward))
            &key (test (lambda (character)
                         (find character ")\""))))
    (edit::apply-at-cursor-until
     (lambda (cursor item)
       (declare (ignore item))
       (edit:move-item-backward cursor))
     cursor #'cluffer:beginning-of-buffer-p
     (complement test) #'edit::item-before-cursor*)))

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

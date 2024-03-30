;;;; package.lisp --- Utilities used in tests of the expression module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.expression.test)

;;; Helper functions

(defun decode-position (source-string position)
  (let* ((line     (count #\Newline source-string :end position))
         (newline  (position #\Newline source-string :end      position
                                                     :from-end t))
         (new-line (if newline
                       (1+ newline)
                       0))
         (column   (- position new-line)))
    (values line column)))

;;; Specialized CST classes
;;;
;;; This provides a very limited syntax tree for testing purposes. The
;;; most severe restriction is that the tree structure matches the CST
;;; structure which means, for example, that #(1 2 3) (which is an
;;; atom) is represented as a leaf instead of having three children.

(defclass line+column-cst (cst:cst) ())

(defmethod x:range ((expression line+column-cst))
  (values-list (cst:source expression)))

(defmethod x:children ((expression line+column-cst))
  '())

(defclass line+column-atom-cst (line+column-cst cst:atom-cst) ())

(defclass line+column-cons-cst (line+column-cst cst:cons-cst) ())

(defmethod x:children ((expression line+column-cons-cst))
  (let ((result '()))
    (labels ((rec (node)
               (etypecase node
                 (cst:cons-cst
                  (push (cst:first node) result)
                  (rec (cst:rest node)))
                 (cst:atom-cst
                  (unless (cst:null node)
                    (push node result))))))
      (rec expression))
    (nreverse result)))

;;; Specialized Eclector client

(defclass client (eclector.concrete-syntax-tree:cst-client)
  ((%source-string :initarg :source-string
                   :reader  source-string)))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result t) (children t) (source t))
  (let* ((result        (call-next-method))
         (source-string (source-string client))
         (source        (destructuring-bind (start . end) (cst:source result)
                          (multiple-value-call #'list
                            (decode-position source-string start)
                            (decode-position source-string end)))))
    (typecase result
      (cst:atom-cst
       (change-class result 'line+column-atom-cst :source source))
      (cst:cons-cst
       (change-class result 'line+column-cons-cst :source source))
      (t
       result))))

;;; Specialized buffer class

(defclass test-buffer (text.editing.test::test-buffer)
  ((%cst :accessor %cst
         :initform nil)))

(defun cst (buffer)
  (or (%cst buffer)
      (setf (%cst buffer)
            (let* ((string (buffer-string buffer))
                   (client (make-instance 'client :source-string string)))
              ;; Method on
              ;; `x:map-expressions-containing-cursor-using-buffer'
              ;; does not handle lines.
              (assert (null (position #\Newline string)))
              (with-input-from-string (stream string)
                (loop :for expression = (eclector.parse-result:read
                                         client stream nil stream)
                      :until (eq expression stream)
                      :collect expression))))))

(defmethod x:map-expressions-containing-cursor-using-buffer
    ((function t) (buffer test-buffer) (cursor cluffer:cursor) (syntax-tree t)
     &key (start-relation '<=) (end-relation '<))
  (let* ((cst           (cst buffer))
         (cursor-column (cluffer:cursor-position cursor)))
    (labels ((rec (node)
               (multiple-value-bind
                     (start-line start-column end-line end-column)
                   (x:range node)
                 (declare (ignore start-line end-line))
                 (when (and (funcall start-relation start-column cursor-column)
                            (funcall end-relation cursor-column end-column))
                   (mapc #'rec (x:children node))
                   (when (listp node) (break))
                   (funcall function node)))))
      (mapc #'rec cst))))

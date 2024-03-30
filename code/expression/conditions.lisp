;;;; conditions.lisp --- Conditions signaled by the expression module.
;;;;
;;;; Copyright (C) 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.expression)

(define-condition expression-condition ()
  ((%expression :initarg :expression
                :reader  expression))
  (:default-initargs
   :expression (a:required-argument :expression)))

(define-condition cursor-not-inside-expression-error (edit:maybe-site-condition
                                                      edit:cursor-condition
                                                      error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<The cursor is not positioned inside an ~
                     expression.~@:>"))))

(define-condition no-expression-before-cursor-error (edit:maybe-site-condition
                                                     edit:cursor-condition
                                                     error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<There is no expression before or surrounding the ~
                     cursor.~@:>"))))

(define-condition no-expression-after-cursor-error (edit:maybe-site-condition
                                                    edit:cursor-condition
                                                    error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<There is no expression after or surrounding the ~
                     cursor.~@:>"))))

(define-condition expression-at-toplevel-error (expression-condition
                                                edit:maybe-site-condition
                                                edit:cursor-condition
                                                error)
  ((%expression ))
  (:report
   (lambda (condition stream)
     (format stream "~@<The expression ~A is already at toplevel.~@:>"
             (expression condition)))))

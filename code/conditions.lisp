;;;; conditions.lisp --- Conditions signaled by the main module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(define-condition editing-condition (condition)
  ())

(define-condition site-condition (editing-condition)
  ((%site :initarg :site
          :reader  site))
  (:default-initargs
   :site (a:required-argument :site)))

;;; Insertion stack

(define-condition insertion-stack-empty-error (site-condition
                                               error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The insertion stack is empty in ~A.~@:>"
             (site condition)))))

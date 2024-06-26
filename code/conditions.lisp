;;;; conditions.lisp --- Conditions signaled by the main module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(define-condition editing-condition (condition)
  ())

(define-condition cursor-condition (editing-condition)
  ((%cursor :initarg :cursor
            :reader  cursor))
  (:default-initargs
   :cursor (a:required-argument :cursor)))

(define-condition site-condition (editing-condition)
  ((%site :initarg :site
          :reader  site)))

(define-condition maybe-site-condition (site-condition)
  ((%site :writer   (setf %site)
          :initform nil)))

(define-condition required-site-condition (site-condition)
  ()
  (:default-initargs
   :site (a:required-argument :site)))

;;; Mark

(define-condition mark-not-set-error (required-site-condition
                                      error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The mark is not set in ~A.~@:>"
             (site condition)))))

(define-condition mark-not-active-error (required-site-condition
                                         error)
  ((%mark :initarg :mark
          :reader  %mark))
  (:default-initargs
   :mark (a:required-argument :mark))
  (:report
   (lambda (condition stream)
     (format stream "~@<The mark ~A is not active in ~A.~@:>"
             (%mark condition) (site condition)))))

(define-condition mark-stack-empty-error (required-site-condition
                                          error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The mark stack is empty in ~A.~@:>"
             (site condition)))))

;;; Insertion stack

(define-condition insertion-stack-empty-error (maybe-site-condition
                                               error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The insertion stack is empty in ~A.~@:>"
             (site condition)))))

;;; Multiple sites

(define-condition multiple-sites-condition (editing-condition)
  ())

(define-condition singular-site-error (multiple-sites-condition
                                       error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<There are no additional sites.~@:>"))))

(define-condition operation-would-collapse-sites-error
    (multiple-sites-condition error)
  ((%operation :initarg :operation
               :reader  operation)
   (%arguments :initarg :arguments
               :reader  arguments))
  (:default-initargs
   :operation (a:required-argument :operation)
   :arguments (a:required-argument :arguments))
  (:report
   (lambda (condition stream)
     (format stream "~@<Performing the operation ~A~@[ ~{~A~^ ~}~] with ~
                     multiple sites would collapse the points of all sites ~
                     to the same location.~@:>"
             (operation condition) (arguments condition)))))

;;; Delimiter conditions

(define-condition delimiter-condition (editing-condition)
  ((%delimiter :initarg :delimiter
               :reader  delimiter))
  (:default-initargs
   :delimiter (a:required-argument :delimiter)))

(define-condition no-closing-delimiter-error (delimiter-condition
                                              maybe-site-condition
                                              cursor-condition
                                              error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<There is no closing ~C after cursor.~@:>"
             (delimiter condition)))))

;;;; conditions.lisp --- Conditions used in the search module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.search)

(define-condition buffer-condition (e:editing-condition) ; TODO move to core?
  ((%buffer :initarg :buffer
            :reader  buffer))
  (:default-initargs
   :buffer (a:required-argument :buffer)))

(define-condition already-in-incremental-search-error (buffer-condition
                                                       error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<An incremental search is already in progress for ~
                     ~A.~@:>"
             (buffer condition)))))

(define-condition not-in-incremental-search-error (buffer-condition
                                                   error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<No incremental search is in progress for ~A.~@:>"
             (buffer condition)))))

(define-condition empty-query-error (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<The query is already empty.~@:>"))))

(define-condition no-next-match-error (text.editing:site-condition
                                       error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<No next match.~@:>"))))

(define-condition no-previous-match-error (text.editing:site-condition
                                           error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<No previous match.~@:>"))))

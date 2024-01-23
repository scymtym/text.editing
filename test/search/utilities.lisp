;;;; utilities.lisp --- Utilities used by test for the search module.
;;;;
;;;; Copyright (C) 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.search.test)

;;; Specialized buffer class

(defclass test-buffer (s:search-state-mixin
                       e:site-mixin
                       cluffer-standard-buffer:buffer)
  ()
  (:default-initargs
   :initial-line (make-instance 'cluffer-standard-line:open-line)))

(defclass multiple-site-test-buffer (s:search-state-mixin
                                     e:multiple-site-mixin
                                     e:site-mixin
                                     cluffer-standard-buffer:buffer)
  ()
  (:default-initargs
   :initial-line (make-instance 'cluffer-standard-line:open-line)))

;;; Assertions

(defun is-match-state (start-line start-column end-line end-column match
                       &optional label case-description)
  (flet ((check-cursor (expected-line expected-column cursor name)
           (is-cursor-state expected-line expected-column
                            (c:line-number cursor) (c:cursor-position cursor)
                            (format nil "match ~A of ~A" name label)
                            case-description)))
    (check-cursor start-line start-column (s:start match) "start")
    (check-cursor end-line   end-column   (s:end match)   "end")))

(defun is-matches-state (expected-matches search-state
                         &rest case-description)
  (let ((expected-count (length expected-matches))
        (matches        (s:matches search-state)))
    (is (= expected-count (length matches)))
    (loop :for match    :across matches
          :for expected :in     expected-matches
          :for i        :from   0
          :for label    =       (format nil "~:R match" (1+ i))
          :for args     =       (append expected
                                        (list match label case-description))
          :do (apply #'is-match-state args))))

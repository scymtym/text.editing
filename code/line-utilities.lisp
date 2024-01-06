;;;; line-utilities.lisp --- Utilities for finding lines.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(defgeneric next-line (thing)
  (:method ((thing c:line))
    (let* ((buffer      (c:buffer thing))
           (line-number (c:line-number thing)))
      (unless (= line-number (1- (c:line-count buffer)))
        (c:find-line buffer (1+ line-number)))))

  (:method ((thing c:cursor))
    (next-line (c:line thing))))

(defgeneric previous-line (thing)
  (:method ((thing c:line))
    (let* ((buffer      (c:buffer thing))
           (line-number (c:line-number thing)))
      (unless (zerop line-number)
        (c:find-line buffer (1- line-number)))))

  (:method ((thing c:cursor))
    (previous-line (c:line thing))))

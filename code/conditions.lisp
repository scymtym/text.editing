;;;; conditions.lisp --- Conditions signaled by the main module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(define-condition editing-condition (condition)
  ())

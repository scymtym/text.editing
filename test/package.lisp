;;;; package.lisp --- Package definition for tests of the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.editing.test
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria)
   (#:c #:cluffer)
   (#:e #:text.editing))

  (:import-from #:fiveam
   #:def-suite
   #:def-suite*
   #:in-suite
   #:test
   #:is
   #:signals
   #:fail)

  ;; Test utilities
  (:export
   #:with-buffer
   #:is-cursor-state
   #:is-site-state
   #:is-site-state*
   #:is-buffer-state
   #:is-buffer-state*)

  (:export
   #:run-tests))

(cl:in-package #:text.editing.test)

(def-suite :text.editing)

(defun run-tests ()
  (5am:run! :text.editing))

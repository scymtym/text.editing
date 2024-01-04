;;;; package.lisp --- Package definition for the text.editing system.
;;;;
;;;; Copyright (C) 2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.editing
  (:use
   #:cl)

  ;; Conditions
  (:export
   #:editing-condition))

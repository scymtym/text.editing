;;;; package.lisp --- Package definition for tests of the search module.
;;;;
;;;; Copyright (C) 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.editing.search.test
  (:use
   #:cl)

  (:local-nicknames
   (#:c #:cluffer)
   (#:e #:text.editing)
   (#:s #:text.editing.search))

  (:import-from #:fiveam
   #:def-suite
   #:def-suite*
   #:in-suite
   #:test
   #:is
   #:signals)

  (:import-from #:text.editing.test
   #:with-buffer
   #:is-buffer-state*
   #:is-cursor-state))

(cl:in-package #:text.editing.search.test)

(def-suite :text.editing.search
  :in :text.editing)

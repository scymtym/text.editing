;;;; package.lisp --- Package definition for tests of the expression module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.editing.expression.test
  (:use
   #:cl)

  (:local-nicknames
   (#:c #:cluffer)
   (#:e #:text.editing)
   (#:x #:text.editing.expression))

  (:import-from #:fiveam
   #:def-suite*
   #:test)

  (:import-from #:text.editing.test
   #:buffer-string
   #:operation-cases))

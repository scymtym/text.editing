;;;; text.editing.asd --- System definitions for the text.editing library.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "text.editing"
  :description "Protocols and implementations for text editing operations."
  :license     "LGPLv3"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("alexandria"
                "utilities.print-items"

                "cluffer")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file "package")
                              (:file "types")
                              (:file "protocol")
                              (:file "conditions")

                              (:file "direction")
                              (:file "line-utilities")
                              (:file "cursor-utilities")

                              (:file "predicates")
                              (:file "unit")

                              (:file "insertion-stack")
                              (:file "site")
                              (:file "buffer-mixins")
                              ;; Operations
                              (:file "motion")
                              (:file "insertion")
                              (:file "deletion")
                              (:file "items")
                              (:file "marking")
                              (:file "copying")
                              (:file "changing-case")
                              (:file "transposing")
                              (:file "filling")
                              (:file "commenting")))

                (:module     "search"
                 :pathname   "code/search"
                 :depends-on ("code")
                 :serial     t
                 :components ((:file "package")
                              (:file "conditions")
                              (:file "state")
                              (:file "buffer-mixin")
                              (:file "operations")))

                (:module      "expression"
                 :pathname    "code/expression"
                 :depends-on  ("code")
                 :serial      t
                 :components  ((:file "package")
                               (:file "protocol")
                               (:file "conditions")
                               (:file "utilities")
                               (:file "unit")
                               (:file "operations"))))

  :in-order-to ((test-op (test-op "text.editing/test"))))

(defsystem "text.editing/test"
  :description "Unit tests for the text.editing system."
  :license     "LGPLv3"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de"

  :version     (:read-file-form #1="data/version-string.sexp")
  :depends-on  ("fiveam"

                "concrete-syntax-tree"
                "eclector-concrete-syntax-tree" ; for expression module tests

                (:version "text.editing" (:read-file-form #1#)))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package")
                              (:file "utilities")
                              ;; Operations
                              (:file "motion")
                              (:file "insertion")
                              (:file "deletion")
                              (:file "items")
                              (:file "marking")
                              (:file "copying")
                              (:file "changing-case")
                              (:file "transposing")
                              (:file "filling")))

                (:module     "search"
                 :pathname   "test/search"
                 :depends-on ("test") ; for test utilities
                 :serial     t
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "operations")))

                (:module     "expression"
                 :pathname "test/expression"
                 :depends-on ("test") ; for test utilities
                 :serial     t
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "operations"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:text.editing.test '#:run-tests)))

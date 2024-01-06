;;;; text.editing.asd --- System definitions for the text.editing library.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "text.editing"
  :description "Protocols and implementations for text editing operations."
  :license     "LGPLv3"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                "cluffer")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file "package")
                              (:file "types")
                              (:file "protocol")
                              (:file "conditions")

                              (:file "line-utilities")
                              (:file "cursor-utilities"))))

  :in-order-to ((test-op (test-op "text.editing/test"))))

(defsystem "text.editing/test"
  :description "Unit tests for the text.editing system."
  :license     "LGPLv3"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("fiveam"

                (:version "text.editing" (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:text.editing.test '#:run-tests)))

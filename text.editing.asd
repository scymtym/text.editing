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
  :depends-on  ()

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file "package")
                              (:file "protocol")
                              (:file "conditions")))))

;;;; package.lisp --- Package definition for the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.editing
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria)

   (#:c #:cluffer))

  ;; Types
  (:export
   #:line-designator
   #:position-designator)

  ;; Conditions
  (:export
   #:editing-condition)

  ;; Line utilities
  (:export
   #:next-line
   #:previous-line)

  ;; Cursor utilities
  (:export
   #:move-item-forward
   #:move-item-backward

   #:move-cursor-to-line

   #:call-with-attached-cursor
   #:with-attached-cursor
   #:call-with-temporary-cursor
   #:with-temporary-cursor
   #:call-with-cloned-cursor
   #:with-cloned-cursor)

  ;; Extended cursor protocol
  (:export
   #:delete-item-forward
   #:delete-item-backward

   #:clone-cursor
   #:clone-initargs))

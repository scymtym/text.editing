;;;; package.lisp --- Package definition for the expression module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.editing.expression
  (:use
   #:cl)

  (:local-nicknames
   (#:a    #:alexandria)
   (#:c    #:cluffer)
   (#:edit #:text.editing))

  ;; Conditions
  (:export
   #:cursor-not-inside-expression-error
   #:no-expression-before-cursor-error
   #:no-expression-after-cursor-error
   #:expression-at-toplevel-error
   #:expression-does-not-have-children-error
   #:no-expression-before-expression-error
   #:no-expression-after-expression-error)

  ;; Expression node protocol
  (:export
   #:range
   #:children)

  ;; Expression tree protocol
  (:export
   #:map-expressions-containing-cursor
   #:map-expressions-containing-cursor-using-buffer
   #:expressions-containing-cursor

   #:innermost-expression-containing-cursor
   #:outermost-expression-containing-cursor)

  ;; Cursor expression protocol
  (:export
   #:expression)

  ;; Expression unit protocol
  (:export
   #:syntax-tree)

  ;; Units
  (:export
   #:expression
   #:toplevel-expression
   #:region-or-expression)

  ;; Operations
  (:export
   #:raise
   #:splice
   #:split
   #:join

   #:eject
   #:absorb))

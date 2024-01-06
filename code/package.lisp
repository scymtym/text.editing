;;;; package.lisp --- Package definition for the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.editing
  (:use
   #:cl)

  (:shadow
   #:delete)

  (:local-nicknames
   (#:a  #:alexandria)
   (#:pi #:utilities.print-items)

   (#:c  #:cluffer))

  ;; Types
  (:export
   #:line-designator
   #:position-designator)

  ;; Conditions
  (:export
   #:editing-condition

   #:site-condition
   #:site

   #:mark-not-set-error

   #:mark-not-active-error

   #:mark-stack-empty-error

   #:insertion-stack-empty-error

   #:multiple-sites-condition

   #:singular-site-error

   #:operation-would-collapse-sites-error
   #:operation
   #:arguments)

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

  ;; Detach protocol
  (:export
   #:detach)

  ;; Extended cursor protocol
  (:export
   #:delete-item-forward
   #:delete-item-backward

   #:clone-cursor
   #:clone-initargs)

  ;; Predicates
  (:export
   #:punctuationp
   #:whitespacep)

  ;; Unit protocol
  (:export
   #:apply-from-cursor
   #:item-transformer)

  ;; Units
  (:export
   #:unit
   #:all-units

   #:region-unit
   #:region
   #:region-or
   #:fallback ; reader

   #:item
   #:region-or-item

   #:line   #:semi-line   #:line-boundary
   #:buffer #:semi-buffer #:buffer-boundary

   #:word
   #:sentence
   #:paragraph)

  ;; Insertion stack protocol
  (:export
   #:forward
   #:backward
   #:insertion

   #:entry-count
   #:top-entry
   #:find-entry
   #:push-entry
   #:pop-entry)

  ;; Preferred column tracking protocol and mixin
  (:export
   #:preferred-column ; also `setf'

   #:preferred-column-tracking-mixin)

  ;; Operation history protocol and mixin
  (:export
   #:most-recent-operation
   #:push-operation

   #:operation-history-mixin)

  ;; Site data protocol and mixin
  (:export
   #:data ; also `setf'

   #:site-data-mixin)

  ;; Site protocol and class
  (:export
   #:point ; also `setf'
   #:mark ; also `setf'
   #:mark-active-p ; also `setf'
   #:mark-stack
   #:insertion-stack

   #:site)

  ;; Buffer protocol and mixins
  (:export
   #:site

   #:site-mixin

   #:other-sites
   #:site-count
   #:map-sites
   #:sites
   #:add-site
   #:remove-site

   #:push-site-at
   #:push-site-relative
   #:pop-site
   #:rotate-sites
   #:remove-other-sites

   #:multiple-site-mixin)

  ;; Operation protocol
  (:export
   #:perform)

  ;; Motion
  (:export
   #:move

   #:back-to-indentation)

  ;; Insertion
  (:export
   #:insert-item
   #:insert-newline
   #:insert-items
   #:insert-string)

  ;; Deletion
  (:export
   #:delete

   #:delete-indentation
   #:delete-trailing-whitespace
   #:fixup-whitespace)

  ;; Items functions
  (:export
   #:map-items
   #:items) ; also `setf'

  ;; Marking
  (:export
   #:mark-or-error
   #:activate-mark
   #:deactivate-mark
   #:set-mark
   #:set-mark-or-toggle-active
   #:pop-mark
   #:exchange-point-and-mark
   #:mark-object)

  ;; Copying
  (:export
   #:yank
   #:copy)

  ;; Modification
  (:export
   #:change-case))

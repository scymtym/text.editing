;;;; package.lisp --- Package definition for the search module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.editing.search
  (:use
   #:cl)

  (:local-nicknames
   (#:a  #:alexandria)
   (#:pi #:utilities.print-items)

   (#:c  #:cluffer)
   (#:e  #:text.editing))

  (:shadow
   #:search
   #:abort)

  ;; Conditions
  (:export
   #:already-in-incremental-search-error
   #:not-in-incremental-search-error
   #:empty-query-error
   #:no-next-match-error
   #:no-previous-match-error)

  ;; Search state protocol
  (:export
   #:start
   #:query
   #:case-mode ; also `setf'

   #:match-count
   #:map-matches
   #:matches
   #:add-match
   #:remove-match
   #:initial-matches
   #:rebuild-state
   #:extend-query
   #:truncate-query

   #:finish
   #:abort

   #:description)

  ;; Match protocol
  (:export
   #:next
   #:previous
   #:start
   #:end)

  ;; Site search state protocol and class
  (:export
   #:start
   #:match

   #:site-search-state)

  ;; Buffer search state protocol and mixin
  (:export
   #:search-state

   #:search-state-mixin)

  ;; High-level interface
  (:export
   #:search
   #:incremental-search
   #:finish-incremental-search
   #:abort-incremental-search
   #:convert-matches-to-sites
   ;; Same as in search state protocol
   ;; #:case-mode ; also `setf'
   ;; #:extend-query
   ;; #:truncate-query
   #:next-match
   #:previous-match))

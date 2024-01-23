;;;; protocol.lisp --- Protocols provided by the search module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.search)

;;; Search state protocol
;;;
;;; Extends detach protocol.

(defgeneric start (search-state))

(defgeneric query (search-state))

(defgeneric case-mode (search-state))

(defgeneric (setf case-mode) (new-value search-state))

(defgeneric match-count (search-state))

(defgeneric map-matches (function search-state))

(defgeneric matches (search-state))

(defgeneric add-match (search-state buffer start end))

(defgeneric remove-match (search-state buffer match))

(defgeneric initial-matches (search-state))

(defgeneric rebuild-state (search-state))

(defgeneric extend-query (search-state item))

(defgeneric truncate-query (search-state &key count))

(defgeneric finish (search-state))

(defgeneric abort (search-state))

(defgeneric description (search-state &key comment))

;;; Match protocol
;;;
;;; Extends detach protocol.

(defgeneric next (match))

(defgeneric previous (match))

(defgeneric start (match))

(defgeneric end (match))

(defgeneric item-matches-p (state match query-item))

;;; Site search state protocol

;;; (defgeneric start (site-search-state)), overlaps search state protocol

(defgeneric match (site-search-state))

;;; Buffer search state protocol

(defgeneric search-state (buffer))

;;; Operations

(defgeneric search (target query direction))

(defgeneric incremental-search (target direction)
  (:documentation
   "Start incremental search in the underlying buffer of TARGET in DIRECTION.

TARGET can be a buffer a site or a cursor.

Create a buffer search state and assign it to the underlying buffer."))

(defgeneric finish-incremental-search (target)
  (:documentation
   "Finish the incremental search associated with TARGET.

Keep all point cursors at the locations to which they were moved due
to search operations.

TARGET can be a buffer, a site or a cursor. In any of those cases, the
incremental search will include all sites of the underlying buffer.

If there is no incremental search associated with TARGET, signal an
error of type `not-in-incremental-search-error'."))

(defgeneric abort-incremental-search (target)
  (:documentation
   "Abort the incremental search associated with TARGET.

In particular, move all involved point cursors back to the locations
at which they resided before the incremental search started.

TARGET can be a buffer, a site or a cursor. In any of those cases, the
incremental search will include all sites of the underlying buffer.

If there is no incremental search associated with TARGET, signal an
error of type `not-in-incremental-search-error'."))

(defgeneric convert-matches-to-sites (target)
  (:documentation
   "Finish incremental search and add a site for each match in the buffer
designated by TARGET.

If there is no incremental search associated with TARGET, signal an
error of type `not-in-incremental-search-error'."))

;; Same as in search state protocol:
;; (setf case-mode)
;; extend-query
;; truncate-query

(defgeneric next-match (target &key wrap-around)
  (:documentation
   "In the search involving TARGET, move all point cursors to the next match.

TARGET can be a buffer, a site or a cursor. In any of those cases, the
incremental search will include all sites of the underlying buffer.

WRAP-AROUND controls the behavior in case there is no next match when
a point cursor should be moved to the next match. If WRAP-AROUND is
false and there is no next match, signal error of type
`no-next-match-error'.

If there is no incremental search associated with TARGET, signal an
error of type `not-in-incremental-search-error'."))

(defgeneric previous-match (target &key wrap-around)
  (:documentation
   "In the search involving TARGET, move all point cursors to the previous match.

TARGET can be a buffer, a site or a cursor. In any of those cases, the
incremental search will include all sites of the underlying buffer.

WRAP-AROUND controls the behavior in case there is no previous match
when a point cursor should be moved to the previous match. If
WRAP-AROUND is false and there is no previous match, signal error of
type `no-previous-match-error'.

If there is no incremental search associated with TARGET, signal an
error of type `not-in-incremental-search-error'."))

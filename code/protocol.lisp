;;;; protocol.lisp --- Protocols provided by the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Detach protocol

(defgeneric detach (object)
  (:documentation
   "Detach OBJECT from any buffer or line it is currently attached to."))

;;; Extended cursor protocol

(defgeneric delete-item-forward (cursor))

(defgeneric delete-item-backward (cursor))

(defgeneric clone-cursor (cursor &rest initargs &key class &allow-other-keys)
  (:method ((cursor c:cursor) &rest initargs &key (class (class-of cursor)))
    (apply #'make-instance class (append (a:remove-from-plist initargs :class)
                                         (clone-initargs cursor)))))

(defgeneric clone-initargs (cursor)
  (:method-combination append)
  (:method append ((cursor c:cursor))
    '()))

;;; Unit protocol

(defgeneric apply-from-cursor (continuation cursor unit direction)
  (:documentation
   "Repeatedly call CONTINUATION until CURSOR has processed one UNIT in DIRECTION.

CONTINUATION is a function the lambda list of which has to be
compatible with

  (cursor item)

UNIT is the unit in or at or around TARGET that CONTINUATION should be
applied to. Examples of units are items, words, lines and paragraphs.

DIRECTION is the direction in which the processing should be performed
from or around TARGET. Possible values are `:forward' and `:backward'."))

(defgeneric item-transformer (transform direction)
  (:documentation
   "Return a function that transforms items via TRANSFORM."))

;;; Insertion stack protocol

(defgeneric forward (insertion-entry)
  (:documentation
   "Return the sequence of items that have been added to INSERTION-ENTRY
by forward deletion operations."))

(defgeneric (setf forward) (new-value insertion-entry)
  (:documentation
   "Set the sequence of items for forward deletion operations of
INSERTION-ENTRY to NEW-VALUE."))

(defgeneric backward (insertion-entry)
  (:documentation
   "Return the sequence of items that have been added to INSERTION-ENTRY
by backward deletion operations."))

(defgeneric (setf backward) (new-value insertion-entry)
  (:documentation
   "Set the sequence of items for backward deletion operations of
INSERTION-ENTRY to NEW-VALUE."))

(defgeneric insertion (insertion-entry)
  (:documentation
   "Return a sequence of items that should be inserted into a buffer to
conceptually insert INSERTION-ENTRY into that buffer.

The returned sequence is the concatenation of the items of the
`forward' and `backward' sequences of INSERTION-ENTRY in the
appropriate order."))

(defgeneric entry-count (insertion-stack)
  (:documentation
   "Return number of insertion entries in @var{insertion-stack}."))

(defgeneric top-entry (insertion-stack)
  (:documentation
   "Return the top entry in INSERTION-STACK or `nil'.

The `forward', `backward' and `insertion' functions can be applied to
the returned object."))

(defgeneric find-entry (index insertion-stack)
  (:documentation
   "Return the entry at INDEX in INSERTION-STACK.

The `forward', `backward' and `insertion' functions can be applied to
the returned object."))

(defgeneric push-entry (insertion-stack)
  (:documentation
   "Add a new entry to INSERTION-STACK and return it the new entry."))

(defgeneric pop-entry (insertion-stack)
  (:documentation
   "Remove the top entry from INSERTION-STACK and return the removed entry.

If INSERTION-STACK is empty, signal an error of type
INSERTION-STACK-EMPTY-ERROR."))

;;; Preferred column protocol

(defgeneric preferred-column (site)
  (:documentation
   "Return the column number in which the point of SITE should reside by default.

The point cursor should be placed in that column or the closest
existing column of the current line when the point cursor moves
between lines without moving within any line."))

(defgeneric (setf preferred-column) (new-value site)
  (:documentation
   "Set the column number in which the point of SITE should reside by default to NEW-VALUE."))

;;; Operation history protocol

(defgeneric most-recent-operation (site)
  (:documentation
   "Return the most recent operation of SITE or `nil'."))

(defgeneric push-operation (operation site)
  (:documentation
   "Push OPERATION into the operation history of SITE."))

;;; Site data protocol

(defgeneric data (key site)
  (:documentation
   "Return data associated with SITE for KEY or `nil'."))

(defgeneric (setf data) (new-value key site)
  (:documentation
   "Set data associated with SITE for KEY to NEW-VALUE.

If NEW-VALUE is `nil' remove the data for KEY from SITE."))

;;; Site protocol
;;;
;;; Includes the detach protocol

(defgeneric point (site)
  (:documentation
   "Return the point cursor of SITE.

The returned object is a cluffer cursor."))

(defgeneric (setf point) (new-value site)
  (:documentation
   "Set the point cursor of SITE to NEW-VALUE."))

(defgeneric mark (site)
  (:documentation
   "Return the mark cursor of SITE or `nil'.

The returned object is a cluffer cursor."))

(defgeneric (setf mark) (new-value site)
  (:documentation
   "Set the mark cursor of SITE to NEW-VALUE."))

(defgeneric mark-active-p (site)
  (:documentation
   "Indicate whether the mark cursor of SITE is active."))

(defgeneric (setf mark-active-p) (new-value site)
  (:documentation
   "Change whether the mark cursor of SITE is active.

NEW-VALUE is a generalized Boolean."))

(defgeneric mark-stack (site)
  (:documentation
   "Return the mark stack of SITE."))

(defgeneric insertion-stack (site)
  (:documentation
   "Return the insertion stack of SITE."))

;;; Buffer protocol

(defgeneric site (buffer)
  (:documentation
   "Return the primary site of BUFFER."))

(defgeneric (setf site) (new-value buffer))

;;; Multiple sites protocol

(defgeneric site-count (buffer)
  (:documentation
   "Return the total number of sites that are attached to BUFFER.

The returned count includes the primary site."))

(defgeneric map-sites (function buffer)
  (:documentation
   "Call FUNCTION with each site that is attached to BUFFER."))

(defgeneric sites (buffer)
  (:documentation
   "Return the sequence of all sites which are attached to BUFFER."))

(defgeneric add-site (site buffer))

(defgeneric remove-site (site buffer))

(defgeneric push-site-at (buffer line position))

(defgeneric push-site-relative (buffer unit direction))

(defgeneric pop-site (buffer))

(defgeneric rotate-sites (buffer direction))

(defgeneric other-sites (buffer)
  (:documentation
   "Return a sequence of secondary sites for BUFFER."))

(defgeneric remove-other-sites (buffer)
  (:documentation
   "Remove all secondary sites from BUFFER."))

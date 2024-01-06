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

;;; Operation protocol

(defgeneric perform (target operation &rest operation-arguments)
  (:documentation
   "Perform OPERATION with OPERATION-ARGUMENTS in or on TARGET.

TARGET is the object in or at or on which the operation should be
performed such as a buffer or a cursor.

OPERATION designates a function which performs the desired operation
when called with a target object (not necessarily TARGET) as the first
argument. The target object in the call to OPERATION may be different
from TARGET when methods on this generic function translate an
operation on one target object to one or more operations on other
target objects. For example, an operation on a buffer is commonly
translated to one operation on each site of the buffer and further to
one operation on the point cursor of each site of the buffer.

OPERATION-ARGUMENTS is a list of additional arguments that should be
passed to the function designated by OPERATION.

This function generally returns the values returned by the OPERATION
call. Similarly, calls to this function may signal any condition that
may be signaled by the OPERATION call. However, if TARGET is a buffer
and multiple sites exist, a different convention may be used in order
to return one result for each site or bundle conditions for multiple
sites in a single condition."))

;;; Default behavior

(defmethod perform ((target c:buffer) (operation t) &rest operation-arguments)
  (apply #'perform (site target) operation operation-arguments))

(defmethod perform ((target c:cursor) (operation t) &rest operation-arguments)
  (apply operation target operation-arguments))

(defmacro operates-on-site (operation)
  `(defmethod perform ((target site) (operation (eql ',operation))
                       &rest operation-arguments)
     ;; Call the operation with the site instead of the point cursor of
     ;; the site.
     (apply (function ,operation) target operation-arguments)))

;;; Operations

;;; Motion Operations

(defgeneric move (cursor unit direction)
  (:documentation
   "Move CURSOR in DIRECTION by one UNIT, return CURSOR.

CURSOR is an attached Cluffer cursor that should be moved.

UNIT is a unit of movement such as `:item', `:word', `:expression'.

DIRECTION is either `:forward' or `:backward'."))

;;; Insertion Operations

(defgeneric insert-item (cursor item)
  (:documentation
   "Insert ITEM at CURSOR."))

(defgeneric insert-newline (cursor)
  (:documentation
   "Split the current line at the position of CURSOR."))

(defgeneric insert-items (cursor items &key start end)
  (:documentation
   "Insert the items in ITEMS at CURSOR.

START and END, when supplied, select a sub-sequence of ITEMS."))

;;; Deletion operations

(defgeneric delete (cursor unit direction)
  (:documentation
   "Delete items from CURSOR in DIRECTION for one UNIT.

CURSOR is an attached Cluffer cursor.

UNIT is a unit of movement such as `item', `word', `expression'.

DIRECTION is either `:forward' or `:backward'."))

(defgeneric delete-indentation (cursor)
  (:documentation
   "Join previous and current line, delete whitespace before and after CURSOR.

Keep a single space character unless the delete placed CURSOR on an
empty line."))

(defgeneric delete-trailing-whitespace (cursor)
  (:documentation
   "Delete trailing whitespace from buffer lines.

CURSOR determines the first line to be processed. All subsequent lines
to the end of the buffer are processed after that."))

(defgeneric fixup-whitespace (cursor)
  (:documentation
   "Delete consecutive whitespace before and after CURSOR in the
current line.

Keep a single space character unless the deletion placed CURSOR at the
beginning of the line."))

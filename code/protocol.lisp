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
    (let ((initargs (append (a:remove-from-plist initargs :class :attach)
                            (clone-initargs cursor))))
      (apply #'make-instance class initargs)))
  (:method :around ((cursor c:cursor) &key (attach t) line position)
    (let ((new-cursor (call-next-method)))
      (when attach
        (let ((line     (or line     (c:line cursor)))
              (position (or position (c:cursor-position cursor))))
          (c:attach-cursor new-cursor line position)))
      new-cursor)))

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
`insertion-stack-empty-error'."))

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

UNIT is a unit of movement such as `item' or `word'.

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

UNIT is a unit of movement such as `item' or `word'.

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

;;; Items functions

(defgeneric map-items (function cursor unit direction)
  (:documentation
   "Call FUNCTION with each item in the sub-sequence of buffer items
indicated by CURSOR, UNIT and DIRECTION."))

(defgeneric items (cursor unit direction)
  (:documentation
   "Return a `cl:sequence' containing the sub-sequence of buffer items
indicated by CURSOR, UNIT and DIRECTION."))

(defgeneric (setf items) (new-value cursor unit direction)
  (:documentation
   "Replace the sub-sequence of buffer items indicated by CURSOR,
UNIT and DIRECTION by the items in the `cl:sequence' NEW-VALUE."))

;;; Marking Operations

(defgeneric mark-or-error (object)
  (:documentation
   "Return the mark cursor of OBJECT or signal an error.

If the mark cursor of OBJECT is not set, signal an error of type
MARK-NOT-SET-ERROR."))

(defgeneric activate-mark (site)
  (:documentation
   "Set the state of the mark cursor of SITE to active.

Signal an error of type `mark-not-set-error' if the mark of SITE is
not set."))

(defgeneric deactivate-mark (site)
  (:documentation
   "Set the state of the mark cursor of SITE to inactive."))

(defgeneric set-mark (site)
  (:documentation
   "Set the mark cursor of SITE to the position of the point cursor.

Push the current mark cursor, if any, onto the mark stack, set a new
mark cursor and move it to the position of the point cursor. Activate
the mark.

Return the new mark cursor."))

(defgeneric set-mark-or-toggle-active (site)
  (:documentation
   "Set the mark cursor of SITE or toggle its active state.

If the previous command was not `set-mark-or-toggle-active', then push
the current mark cursor of SITE onto the mark stack, set a new mark
cursor and move it to the position of the point cursor.

If the previous command was `set-mark-or-toggle-active', then toggle
the active state of the mark cursor of SITE.

Return two values: a Boolean which indicates whether a new mark cursor
was set and another Boolean which indicates whether the mark is
active."))

(defgeneric pop-mark (site)
  (:documentation
   "Pop a mark off the mark stack of SITE and move the point cursor to it.

Destroy the current mark of SITE, if any.

Return the popped mark cursor.

Signal an error of type `mark-stack-empty' if the mark stack of SITE
is empty."))

(defgeneric exchange-point-an-dmark (site)
  (:documentation
   "Exchange the locations of point and mark of SITE.

Signal an error of type `mark-not-set-error' if the mark of SITE is
not set."))

(defgeneric mark-object (site unit direction)
  (:documentation
   "Set region of SITE according to UNIT and DIRECTION.

Leave the point cursor of SITE at its current location. Ensure the
mark is set and active (see below) and move the mark cursor according
to UNIT and DIRECTION.

If the mark of SITE is not set, set a new mark cursor at the location
of the point cursor and activate it. Then apply the motion according
to UNIT and DIRECTION.

If the mark of SITE is set but not active, activate the mark cursor
and move it to the location of the point cursor. Then apply the motion
according to UNIT and DIRECTION.

If the mark of SITE is set and active, just apply the motion according
to UNIT and DIRECTION. This last case allows extending the region by
marking subsequent objects."))

;;; Copy, kill and yank operations

(defgeneric yank (site direction &key pop)
  (:documentation
   "Insert top insertion stack entry of SITE at the point of SITE.

Insert the items from the top entry of the insertion stack of SITE
before or after the point cursor of SITE.

DIRECTION controls whether the items are inserted before or after the
point cursor, or equivalently, whether the point cursor moves to the
beginning or end of the inserted items after the insertion."))

(defgeneric copy (site unit direction)
  (:documentation
   "Copy items according at SITE according to UNIT and DIRECTION.

The items indicated by the point cursor of SITE, UNIT and DIRECTION
are copied into either the current top entry of the insertion stack of
SITE or a new entry that is first pushed onto the insertion stack.

Whether a new entry should be created is decided according to an
internal protocol that may be exported at some later time."))

;;; Case changing operations

(defgeneric change-case (cursor unit direction case)
  (:documentation
   "Change the case of items to CASE from CURSOR in DIRECTION for one UNIT.

CURSOR is an attached Cluffer cursor.

UNIT is a unit of movement such as `item', `word', or `expression'.

DIRECTION is either `:forward' or `:backward'.

CASE is one of `:down', `:up' and `:capital'.

Change the case of items between CURSOR and a fictional cursor C2 that
is obtained by cloning CURSOR and applying (move C2 UNIT
DIRECTION). The case of an item is changed by calling `char-downcase'
if CASE is `:down', `char-upcase' if CASE is `:up' and in a fashion
analogous to `string-capitalize' if CASE is `:capital'."))

;;; Transposing operations

(defgeneric transpose (cursor unit direction)
  (:documentation
   "Exchange the sequences of items defined by UNIT before and after CURSOR.

If CURSOR is within a UNIT, it is first moved to the boundary of that
UNIT according to DIRECTION.

DIRECTION is either `:forward' or `:backward' and controls where CURSOR
is positioned after the operation."))

;;; Filling operations

(defgeneric insert-words-fill (cursor words
                               &key prefix suffix per-line-prefix fill-column))

(defgeneric fill-words (start-cursor end-cursor words
                        &rest args
                        &key prefix suffix per-line-prefix fill-column))

;;; Commenting operations

(defgeneric comment (cursor unit direction &key comment-syntax))

(defgeneric uncomment (cursor unit direction))

;;; Delimiter pairs

(defgeneric insert-delimiter-pair (cursor opening &key closing))

(defgeneric maybe-move-past-closing-delimiter (cursor closing &key whitespace))

(defgeneric move-past-closing-delimiter (cursor closing &key whitespace))

(defgeneric move-past-closing-delimiter-or-insert-delimiter-pair
    (cursor delimiter &key whitespace))

(defgeneric delete-delimiter-pair-or-item (cursor direction &key if-not-empty))

(defgeneric surround-with-delimiter-pair (cursor unit direction opening
                                          &key closing count))

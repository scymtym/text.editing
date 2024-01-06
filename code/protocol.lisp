;;;; protocol.lisp --- Protocols provided by the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

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

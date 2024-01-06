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

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

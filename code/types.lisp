;;;; types.lisp --- Types used in the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(deftype line-designator ()
  '(or (integer 0) c:line (member :first :last)))

(deftype position-designator ()
  '(or (integer 0) (member :start :end)))

(deftype direction ()
  '(member :forward :backward))

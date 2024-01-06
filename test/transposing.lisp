;;;; transposing.lisp --- Tests for transposing operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.transposing
  :in :text.editing)

(test transpose.smoke
  "Smoke test for the `transpose' operation."
  (operation-cases (e:transpose)
   ((e:item (:forward :backward))
    ("↑Hello world.¶line 2" 'c:beginning-of-buffer 'c:beginning-of-buffer)
    ("Hel↑lo world.¶line 2" "Hell↑o world.¶line 2" "He↑llo world.¶line 2")
    ("Hello↑ world.¶line 2" "Hell o↑world.¶line 2" "Hell↑ oworld.¶line 2")
    ("Hello world.↑¶line 2" "Hello world¶.↑line 2" "Hello world↑¶.line 2")
    ("Hello world.¶↑line 2" "Hello world.l¶↑ine 2" "Hello world.↑l¶ine 2")
    ("Hello world.¶line 2↑" 'c:end-of-buffer       'c:end-of-buffer))))

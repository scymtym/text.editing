;;;; changing-case.lisp --- Tests for case changing operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.changing-case
  :in :text.editing)

(test change-case.smoke
  "Smoke test for the `change-case' operation."
  (operation-cases (e:change-case)
    ((e:item (:forward :backward) (:up :down :capital))
     ("↑Hello world.¶line 2" "H↑ello world.¶line 2" "h↑ello world.¶line 2" "H↑ello world.¶line 2"
                             'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("Hel↑lo world.¶line 2" "HelL↑o world.¶line 2" "Hell↑o world.¶line 2" "HelL↑o world.¶line 2"
                             "He↑Llo world.¶line 2" "He↑llo world.¶line 2" "He↑Llo world.¶line 2")
     ("Hello↑ world.¶line 2" "Hello ↑world.¶line 2" "Hello ↑world.¶line 2" "Hello ↑world.¶line 2"
                             "Hell↑O world.¶line 2" "Hell↑o world.¶line 2" "Hell↑O world.¶line 2")
     ("Hello world.↑¶line 2" "Hello world.¶↑line 2" "Hello world.¶↑line 2" "Hello world.¶↑line 2"
                             "Hello world↑.¶line 2" "Hello world↑.¶line 2" "Hello world↑.¶line 2")
     ("Hello world.¶↑line 2" "Hello world.¶L↑ine 2" "Hello world.¶l↑ine 2" "Hello world.¶L↑ine 2"
                             "Hello world.↑¶line 2" "Hello world.↑¶line 2" "Hello world.↑¶line 2")
     ("Hello world.¶line 2↑" 'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
                             "Hello world.¶line ↑2" "Hello world.¶line ↑2" "Hello world.¶line ↑2"))
    ((e:line (:forward :backward) (:up :down :capital))
     ("↑Hello world.¶line 2" "HELLO WORLD.↑¶line 2" "hello world.↑¶line 2" "Hello World.↑¶line 2"
                             "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
     ("Hel↑lo world.¶line 2" "HELLO WORLD.↑¶line 2" "hello world.↑¶line 2" "Hello World.↑¶line 2"
                             "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
     ("Hello↑ world.¶line 2" "HELLO WORLD.↑¶line 2" "hello world.↑¶line 2" "Hello World.↑¶line 2"
                             "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
     ("Hello world.↑¶line 2" "HELLO WORLD.↑¶line 2" "hello world.↑¶line 2" "Hello World.↑¶line 2"
                             "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
     ("Hello world.¶↑line 2" "Hello world.¶LINE 2↑" "Hello world.¶line 2↑" "Hello world.¶Line 2↑"
                             "Hello world.¶↑LINE 2" "Hello world.¶↑line 2" "Hello world.¶↑linE 2")
     ("Hello world.¶line 2↑" "Hello world.¶LINE 2↑" "Hello world.¶line 2↑" "Hello world.¶Line 2↑"
                             "Hello world.¶↑LINE 2" "Hello world.¶↑line 2" "Hello world.¶↑linE 2"))
    ((e:semi-line (:forward :backward) (:up :down :capital))
     . #1=(("↑Hello world.¶line 2" "HELLO WORLD.↑¶line 2" "hello world.↑¶line 2" "Hello World.↑¶line 2"
                                   "↑Hello world.¶line 2" "↑Hello world.¶line 2" "↑Hello world.¶line 2")
           ("Hel↑lo world.¶line 2" "HelLO WORLD.↑¶line 2" "Hello world.↑¶line 2" "HelLo World.↑¶line 2"
                                   "↑HELlo world.¶line 2" "↑hello world.¶line 2" "↑heLlo world.¶line 2")
           ("Hello↑ world.¶line 2" "Hello WORLD.↑¶line 2" "Hello world.↑¶line 2" "Hello World.↑¶line 2"
                                   "↑HELLO world.¶line 2" "↑hello world.¶line 2" "↑hellO world.¶line 2")
           ("Hello world.↑¶line 2" "Hello world.↑¶line 2" "Hello world.↑¶line 2" "Hello world.↑¶line 2"
                                   "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
           ("Hello world.¶↑line 2" "Hello world.¶LINE 2↑" "Hello world.¶line 2↑" "Hello world.¶Line 2↑"
                                   "Hello world.¶↑line 2" "Hello world.¶↑line 2" "Hello world.¶↑line 2")
           ("Hello world.¶line 2↑" "Hello world.¶line 2↑" "Hello world.¶line 2↑" "Hello world.¶line 2↑"
                                   "Hello world.¶↑LINE 2" "Hello world.¶↑line 2" "Hello world.¶↑linE 2")))
    #+TODO ((e:line-boundary (:forward :backward) (:up :down :capital)) . #1#)
    ((e:buffer (:forward :backward) (:up :down :capital))
     ("↑Hello world.¶line 2" "HELLO WORLD.¶LINE 2↑" "hello world.¶line 2↑" "Hello World.¶Line 2↑"
                             "↑HELLO WORLD.¶LINE 2" "↑hello world.¶line 2" "↑hellO worlD.¶linE 2")
     ("Hel↑lo world.¶line 2" "HELLO WORLD.¶LINE 2↑" "hello world.¶line 2↑" "Hello World.¶Line 2↑"
                             "↑HELLO WORLD.¶LINE 2" "↑hello world.¶line 2" "↑hellO worlD.¶linE 2")
     ("Hello↑ world.¶line 2" "HELLO WORLD.¶LINE 2↑" "hello world.¶line 2↑" "Hello World.¶Line 2↑"
                             "↑HELLO WORLD.¶LINE 2" "↑hello world.¶line 2" "↑hellO worlD.¶linE 2")
     ("Hello world.↑¶line 2" "HELLO WORLD.¶LINE 2↑" "hello world.¶line 2↑" "Hello World.¶Line 2↑"
                             "↑HELLO WORLD.¶LINE 2" "↑hello world.¶line 2" "↑hellO worlD.¶linE 2")
     ("Hello world.¶↑line 2" "HELLO WORLD.¶LINE 2↑" "hello world.¶line 2↑" "Hello World.¶Line 2↑"
                             "↑HELLO WORLD.¶LINE 2" "↑hello world.¶line 2" "↑hellO worlD.¶linE 2")
     ("Hello world.¶line 2↑" "HELLO WORLD.¶LINE 2↑" "hello world.¶line 2↑" "Hello World.¶Line 2↑"
                             "↑HELLO WORLD.¶LINE 2" "↑hello world.¶line 2" "↑hellO worlD.¶linE 2"))
    ((e:semi-buffer (:forward :backward) (:up :down :capital))
     . #2=(("↑Hello world.¶line 2" "HELLO WORLD.¶LINE 2↑" "hello world.¶line 2↑" "Hello World.¶Line 2↑"
                                   "↑Hello world.¶line 2" "↑Hello world.¶line 2" "↑Hello world.¶line 2")
           ("Hel↑lo world.¶line 2" "HelLO WORLD.¶LINE 2↑" "Hello world.¶line 2↑" "HelLo World.¶Line 2↑"
                                   "↑HELlo world.¶line 2" "↑hello world.¶line 2" "↑heLlo world.¶line 2")
           ("Hello↑ world.¶line 2" "Hello WORLD.¶LINE 2↑" "Hello world.¶line 2↑" "Hello World.¶Line 2↑"
                                   "↑HELLO world.¶line 2" "↑hello world.¶line 2" "↑hellO world.¶line 2")
           ("Hello world.↑¶line 2" "Hello world.¶LINE 2↑" "Hello world.¶line 2↑" "Hello world.¶Line 2↑"
                                   "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
           ("Hello world.¶↑line 2" "Hello world.¶LINE 2↑" "Hello world.¶line 2↑" "Hello world.¶Line 2↑"
                                   "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
           ("Hello world.¶line 2↑" "Hello world.¶line 2↑" "Hello world.¶line 2↑" "Hello world.¶line 2↑"
                                   "↑HELLO WORLD.¶LINE 2" "↑hello world.¶line 2" "↑hellO worlD.¶linE 2")))
    #+TODO ((e:buffer-boundary (:forward :backward) (:up :down :capital)) . #2#)
    ;; Prose units
    ((e:word (:forward :backward) (:up :down :capital))
     ("↑Hello world.¶line 2" "HELLO↑ world.¶line 2" "hello↑ world.¶line 2" "Hello↑ world.¶line 2"
                             'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("Hel↑lo world.¶line 2" "HelLO↑ world.¶line 2" "Hello↑ world.¶line 2" "HelLo↑ world.¶line 2"
                             "↑HELlo world.¶line 2" "↑hello world.¶line 2" "↑heLlo world.¶line 2")
     ("Hello↑ world.¶line 2" "Hello WORLD↑.¶line 2" "Hello world↑.¶line 2" "Hello World↑.¶line 2"
                             "↑HELLO world.¶line 2" "↑hello world.¶line 2" "↑hellO world.¶line 2")
     ("Hello world.↑¶line 2" "Hello world.¶LINE↑ 2" "Hello world.¶line↑ 2" "Hello world.¶Line↑ 2"
                             "Hello ↑WORLD.¶line 2" "Hello ↑world.¶line 2" "Hello ↑worlD.¶line 2")
     ("Hello world.¶↑line 2" "Hello world.¶LINE↑ 2" "Hello world.¶line↑ 2" "Hello world.¶Line↑ 2"
                             "Hello ↑WORLD.¶line 2" "Hello ↑world.¶line 2" "Hello ↑worlD.¶line 2")
     ("Hello world.¶line 2↑" 'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
                             "Hello world.¶line ↑2" "Hello world.¶line ↑2" "Hello world.¶line ↑2"))
    ((e:sentence (:forward :backward) (:up :down :capital))
     ("↑Hello world.¶line 2" "HELLO WORLD.↑¶line 2" "hello world.↑¶line 2" "Hello World.↑¶line 2"
                             'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("Hel↑lo world.¶line 2" "HelLO WORLD.↑¶line 2" "Hello world.↑¶line 2" "HelLo World.↑¶line 2"
                             "↑HELlo world.¶line 2" "↑hello world.¶line 2" "↑heLlo world.¶line 2")
     ("Hello↑ world.¶line 2" "Hello WORLD.↑¶line 2" "Hello world.↑¶line 2" "Hello World.↑¶line 2"
                             "↑HELLO world.¶line 2" "↑hello world.¶line 2" "↑hellO world.¶line 2")
     ("Hello world.↑¶line 2" "Hello world.¶LINE 2↑" "Hello world.¶line 2↑" "Hello world.¶Line 2↑"
                             "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
     ("Hello world.¶↑line 2" "Hello world.¶LINE 2↑" "Hello world.¶line 2↑" "Hello world.¶Line 2↑"
                             "↑HELLO WORLD.¶line 2" "↑hello world.¶line 2" "↑hellO worlD.¶line 2")
     ("Hello world.¶line 2↑" 'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
                             "Hello world.¶↑LINE 2" "Hello world.¶↑line 2" "Hello world.¶↑linE 2"))
    ((e:paragraph (:forward :backward) (:up :down :capital))
     ("↑Hello¶world.¶¶line 2" "HELLO¶WORLD.¶¶↑line 2" "hello¶world.¶¶↑line 2" "Hello¶World.¶¶↑line 2"
                              'c:beginning-of-buffer  'c:beginning-of-buffer  'c:beginning-of-buffer)
     ("Hel↑lo¶world.¶¶line 2" "HelLO¶WORLD.¶¶↑line 2" "Hello¶world.¶¶↑line 2" "HelLo¶World.¶¶↑line 2"
                              "↑HELlo¶world.¶¶line 2" "↑hello¶world.¶¶line 2" "↑heLlo¶world.¶¶line 2")
     ("Hello↑¶world.¶¶line 2" "Hello¶WORLD.¶¶↑line 2" "Hello¶world.¶¶↑line 2" "Hello¶World.¶¶↑line 2"
                              "↑HELLO¶world.¶¶line 2" "↑hello¶world.¶¶line 2" "↑hellO¶world.¶¶line 2")
     ("Hello¶world.↑¶¶line 2" "Hello¶world.¶¶↑line 2" "Hello¶world.¶¶↑line 2" "Hello¶world.¶¶↑line 2"
                              "↑HELLO¶WORLD.¶¶line 2" "↑hello¶world.¶¶line 2" "↑hellO¶worlD.¶¶line 2")
     ("Hello¶world.¶¶↑line 2" "Hello¶world.¶¶LINE 2↑" "Hello¶world.¶¶line 2↑" "Hello¶world.¶¶Line 2↑"
                              "↑HELLO¶WORLD.¶¶line 2" "↑hello¶world.¶¶line 2" "↑hellO¶worlD.¶¶line 2")
     ("Hello¶world.¶¶line 2↑" 'c:end-of-buffer        'c:end-of-buffer        'c:end-of-buffer
                              "Hello¶world.¶¶↑LINE 2" "Hello¶world.¶¶↑line 2" "Hello¶world.¶¶↑linE 2"))))

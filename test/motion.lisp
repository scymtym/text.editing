;;;; motion.lisp --- Tests for motion operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.motion
  :in :text.editing)

(test move.smoke
  "Smoke test for the `move' operation."
  (operation-cases (e:move)
   ;; Buffer structure units
   ((e:item (:forward :backward))
    ("↑hello world.¶line 2" "h↑ello world.¶line 2" 'c:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hell↑o world.¶line 2" "he↑llo world.¶line 2")
    ("hello↑ world.¶line 2" "hello ↑world.¶line 2" "hell↑o world.¶line 2")
    ("hello world.↑¶line 2" "hello world.¶↑line 2" "hello world↑.¶line 2")
    ("hello world.¶↑line 2" "hello world.¶l↑ine 2" "hello world.↑¶line 2")
    ("hello world.¶line 2↑" 'c:end-of-buffer       "hello world.¶line ↑2"))
   ((e:line (:forward :backward))
    ("↑hello world.¶line 2" "hello world.¶↑line 2" 'c:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hello world.¶lin↑e 2" 'c:beginning-of-buffer)
    ("hello↑ world.¶line 2" "hello world.¶line ↑2" 'c:beginning-of-buffer)
    ("hello world.↑¶line 2" "hello world.¶line 2↑" 'c:beginning-of-buffer)
    ("hello world.¶↑line 2" 'c:end-of-buffer       "↑hello world.¶line 2")
    ("hello world.¶line 2↑" 'c:end-of-buffer       "hello ↑world.¶line 2"))
   ((e:semi-line (:forward :backward))
    . #1=(("↑hello world.¶line 2" "hello world.↑¶line 2" "↑hello world.¶line 2")
          ("hel↑lo world.¶line 2" "hello world.↑¶line 2" "↑hello world.¶line 2")
          ("hello↑ world.¶line 2" "hello world.↑¶line 2" "↑hello world.¶line 2")
          ("hello world.↑¶line 2" "hello world.↑¶line 2" "↑hello world.¶line 2")
          ("hello world.¶↑line 2" "hello world.¶line 2↑" "hello world.¶↑line 2")
          ("hello world.¶line 2↑" "hello world.¶line 2↑" "hello world.¶↑line 2")))
   ((e:line-boundary (:forward :backward)) . #1#)
   ((e:buffer (:forward :backward))
    . #2=(("↑hello world.¶line 2" "hello world.¶line 2↑" "↑hello world.¶line 2")
          ("hel↑lo world.¶line 2" "hello world.¶line 2↑" "↑hello world.¶line 2")
          ("hello↑ world.¶line 2" "hello world.¶line 2↑" "↑hello world.¶line 2")
          ("hello world.↑¶line 2" "hello world.¶line 2↑" "↑hello world.¶line 2")
          ("hello world.¶↑line 2" "hello world.¶line 2↑" "↑hello world.¶line 2")
          ("hello world.¶line 2↑" "hello world.¶line 2↑" "↑hello world.¶line 2")))
   ((e:semi-buffer (:forward :backward)) . #2#)
   ((e:buffer-boundary (:forward :backward)) . #2#)
   ;; Prose units
   ((e:word (:forward :backward))
    ("↑hello world.¶line 2" "hello↑ world.¶line 2" 'c:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hello↑ world.¶line 2" "↑hello world.¶line 2")
    ("hello↑ world.¶line 2" "hello world↑.¶line 2" "↑hello world.¶line 2")
    ("hello world.↑¶line 2" "hello world.¶line↑ 2" "hello ↑world.¶line 2")
    ("hello world.¶↑line 2" "hello world.¶line↑ 2" "hello ↑world.¶line 2")
    ("hello world.¶line 2↑" 'c:end-of-buffer       "hello world.¶line ↑2"))
   ((e:sentence (:forward :backward))
    ("↑hello world.¶line 2" "hello world.↑¶line 2" 'c:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hello world.↑¶line 2" "↑hello world.¶line 2")
    ("hello↑ world.¶line 2" "hello world.↑¶line 2" "↑hello world.¶line 2")
    ("hello world.↑¶line 2" "hello world.¶line 2↑" "↑hello world.¶line 2")
    ("hello world.¶↑line 2" "hello world.¶line 2↑" "↑hello world.¶line 2")
    ("hello world.¶line 2↑" 'c:end-of-buffer       "hello world.¶↑line 2"))
   ((e:paragraph (:forward :backward))
    ("↑hello¶world.¶¶line 2" "hello¶world.¶¶↑line 2" 'c:beginning-of-buffer)
    ("hel↑lo¶world.¶¶line 2" "hello¶world.¶¶↑line 2" "↑hello¶world.¶¶line 2")
    ("hello↑¶world.¶¶line 2" "hello¶world.¶¶↑line 2" "↑hello¶world.¶¶line 2")
    ("hello¶world.↑¶¶line 2" "hello¶world.¶¶↑line 2" "↑hello¶world.¶¶line 2")
    ("hello¶world.¶¶↑line 2" "hello¶world.¶¶line 2↑" "↑hello¶world.¶¶line 2")
    ("hello¶world.¶¶line 2↑" 'c:end-of-buffer        "hello¶world.¶¶↑line 2"))))

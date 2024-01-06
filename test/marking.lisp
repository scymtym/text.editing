;;;; marking.lisp --- Tests for marking operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.marking
  :in :text.editing)

(test activate-mark.smoke
  "Smoke test for the `activate-mark' operation."
  (operation-cases (e:activate-mark :target :buffer)
   (()
    ("↑"    'e:mark-not-set-error)
    ("↑↓"    "↑↧" )
    ("↑↧"    "↑↧" )
    ("foo↑↓" "foo↑↧")
    ("foo↑↧" "foo↑↧")
    ("↑bar↓" "↑bar↧")
    ("↑bar↧" "↑bar↧"))))

(test deactivate-mark.smoke
  "Smoke test for the `deactivate-mark' operation."
  (operation-cases (e:deactivate-mark :target :buffer)
   (()
    ("↑"     "↑")
    ("↑↓"    "↑↓" )
    ("↑↧"    "↑↓" )
    ("foo↑↓" "foo↑↓")
    ("foo↑↧" "foo↑↓")
    ("↑bar↓" "↑bar↓")
    ("↑bar↧" "↑bar↓"))))

(test set-mark-or-toggle-activate.smoke
  "Smoke test for the `set-mark-or-toggle-activate' operation."
  (operation-cases (e:set-mark-or-toggle-active :target :buffer)
   (()
    ("↑"     "↑↧")
    ("↑↓"    "↑↧" )
    ("↑↧"    "↑↧" )
    ("foo↑↓" "foo↑↧")
    ("foo↑↧" "foo↑↧")
    ("↑bar↓" "↑↧bar")
    ("↑bar↧" "↑↧bar"))))

(test exchange-point-and-mark
  "Smoke test for the `exchange-point-and-mark' operation."
  (operation-cases (e:exchange-point-and-mark :target :buffer)
   (()
    ("↑"     'e:mark-not-set-error)
    ("↑↓"    "↑↓" )
    ("↑↧"    "↑↧" )
    ("foo↑↓" "foo↑↓")
    ("foo↑↧" "foo↑↧")
    ("↑bar↓" "↓bar↑")
    ("↑bar↧" "↧bar↑")
    ("↓bar↑" "↑bar↓")
    ("↧bar↑" "↑bar↧"))))

(test mark-object.smoke
  "Smoke test for the `mark-object' operation."
  (operation-cases (e:mark-object :target :buffer)
   ;; Buffer structure units
   ((e:item (:forward :backward))
    ("↑hello world.¶line 2"  "↑h↧ello world.¶line 2" 'cluffer:beginning-of-buffer)
    ("↑h↓ello world.¶line 2" "↑h↧ello world.¶line 2" 'cluffer:beginning-of-buffer)
    ("↑h↧ello world.¶line 2" "↑he↧llo world.¶line 2" "↑↧hello world.¶line 2")
    ("hel↑lo world.¶line 2"  "hel↑l↧o world.¶line 2" "he↧l↑lo world.¶line 2")
    ("hello↑ world.¶line 2"  "hello↑ ↧world.¶line 2" "hell↧o↑ world.¶line 2")
    ("hello world.↑¶line 2"  "hello world.↑¶↧line 2" "hello world↧.↑¶line 2")
    ("hello world.¶↑line 2"  "hello world.¶↑l↧ine 2" "hello world.↧¶↑line 2")
    ("hello world.¶line 2↑"  'cluffer:end-of-buffer  "hello world.¶line ↧2↑"))
   ((e:line (:forward :backward))
    ("↑hello world.¶line 2" "↑hello world.¶↧line 2" 'cluffer:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hel↑lo world.¶lin↧e 2" 'cluffer:beginning-of-buffer)
    ("hello↑ world.¶line 2" "hello↑ world.¶line ↧2" 'cluffer:beginning-of-buffer)
    ("hello world.↑¶line 2" "hello world.↑¶line 2↧" 'cluffer:beginning-of-buffer)
    ("hello world.¶↑line 2" 'cluffer:end-of-buffer  "↧hello world.¶↑line 2")
    ("hello world.¶line 2↑" 'cluffer:end-of-buffer  "hello ↧world.¶line 2↑"))
   ((e:semi-line (:forward :backward))
    . #1=(("↑hello world.¶line 2" "↑hello world.↧¶line 2" "↑↧hello world.¶line 2")
          ("hel↑lo world.¶line 2" "hel↑lo world.↧¶line 2" "↧hel↑lo world.¶line 2")
          ("hello↑ world.¶line 2" "hello↑ world.↧¶line 2" "↧hello↑ world.¶line 2")
          ("hello world.↑¶line 2" "hello world.↑↧¶line 2" "↧hello world.↑¶line 2")
          ("hello world.¶↑line 2" "hello world.¶↑line 2↧" "hello world.¶↑↧line 2")
          ("hello world.¶line 2↑" "hello world.¶line 2↑↧" "hello world.¶↧line 2↑")))
   ((e:line-boundary (:forward :backward)) . #1#)
   ((e:buffer (:forward :backward))
    . #2=(("↑hello world.¶line 2" "↑hello world.¶line 2↧" "↑↧hello world.¶line 2")
          ("hel↑lo world.¶line 2" "hel↑lo world.¶line 2↧" "↧hel↑lo world.¶line 2")
          ("hello↑ world.¶line 2" "hello↑ world.¶line 2↧" "↧hello↑ world.¶line 2")
          ("hello world.↑¶line 2" "hello world.↑¶line 2↧" "↧hello world.↑¶line 2")
          ("hello world.¶↑line 2" "hello world.¶↑line 2↧" "↧hello world.¶↑line 2")
          ("hello world.¶line 2↑" "hello world.¶line 2↑↧" "↧hello world.¶line 2↑")))
   ((e:semi-buffer (:forward :backward)) . #2#)
   ((e:buffer-boundary (:forward :backward)) . #2#)
   ;; Prose units
   ((e:word (:forward :backward))
    ("↑hello world.¶line 2"  "↑hello↧ world.¶line 2" 'cluffer:beginning-of-buffer)
    ("↑hello↓ world.¶line 2" "↑hello↧ world.¶line 2" 'cluffer:beginning-of-buffer)
    ("↑hello↧ world.¶line 2" "↑hello world↧.¶line 2" "↑↧hello world.¶line 2")
    ("hel↑lo world.¶line 2"  "hel↑lo↧ world.¶line 2" "↧hel↑lo world.¶line 2")
    ("hello↑ world.¶line 2"  "hello↑ world↧.¶line 2" "↧hello↑ world.¶line 2")
    ("hello world.↑¶line 2"  "hello world.↑¶line↧ 2" "hello ↧world.↑¶line 2")
    ("hello world.¶↑line 2"  "hello world.¶↑line↧ 2" "hello ↧world.¶↑line 2")
    ("hello world.¶line 2↑"  'cluffer:end-of-buffer "hello world.¶line ↧2↑"))
   ((e:sentence (:forward :backward))
    ("↑hello world.¶line 2" "↑hello world.↧¶line 2" 'cluffer:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hel↑lo world.↧¶line 2" "↧hel↑lo world.¶line 2")
    ("hello↑ world.¶line 2" "hello↑ world.↧¶line 2" "↧hello↑ world.¶line 2")
    ("hello world.↑¶line 2" "hello world.↑¶line 2↧" "↧hello world.↑¶line 2")
    ("hello world.¶↑line 2" "hello world.¶↑line 2↧" "↧hello world.¶↑line 2")
    ("hello world.¶line 2↑" 'cluffer:end-of-buffer "hello world.¶↧line 2↑"))
   ((e:paragraph (:forward :backward))
    ("↑hello¶world.¶¶line 2" "↑hello¶world.¶¶↧line 2" 'cluffer:beginning-of-buffer)
    ("hel↑lo¶world.¶¶line 2" "hel↑lo¶world.¶¶↧line 2" "↧hel↑lo¶world.¶¶line 2")
    ("hello↑¶world.¶¶line 2" "hello↑¶world.¶¶↧line 2" "↧hello↑¶world.¶¶line 2")
    ("hello¶world.↑¶¶line 2" "hello¶world.↑¶¶↧line 2" "↧hello¶world.↑¶¶line 2")
    ("hello¶world.¶¶↑line 2" "hello¶world.¶¶↑line 2↧" "↧hello¶world.¶¶↑line 2")
    ("hello¶world.¶¶line 2↑" 'cluffer:end-of-buffer  "hello¶world.¶¶↧line 2↑"))))

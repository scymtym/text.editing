;;;; copying.lisp --- Tests for copy, kill and yank operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.copying
  :in :text.editing)

(test yank.smoke
  "Smoke test for the `yank' operation."
  (operation-cases (e:yank :target :buffer)
   (((:forward :backward))
    ("↑"                     'e:insertion-stack-empty-error 'e:insertion-stack-empty-error)
    ('("↑" :killed "foo")    '("foo↑" :killed "foo")        '("↑foo" :killed "foo"))
    ('("bar↑" :killed "foo") '("barfoo↑" :killed "foo")     '("bar↑foo" :killed "foo"))
    ('("↑bar" :killed "foo") '("foo↑bar" :killed "foo")     '("↑foobar" :killed "foo")))))

(test copy.smoke
  "Smoke test for the `copy' operation."
  (operation-cases (e:copy :target :buffer)
   ;; Buffer structure units
   ((e:item (:forward ; TODO :backward
             ))
    ("↑"                    'c:end-of-buffer)
    ("hel↑lo world.¶line 2" '("hel↑lo world.¶line 2" :killed "l")))
   ((e:line (:forward ; :backward
             ))
    ("hel↑lo world.¶line 2" `("hel↑lo world.¶line 2"
                              :killed ,(format nil "hello world.~%"))))
   ((e:semi-line (:forward ; :backward
             ))
    ("hel↑lo world.¶line 2" '("hel↑lo world.¶line 2" :killed "lo world.")))
   ((e:buffer (:forward ; :backward
             ))
    ("hel↑lo world.¶line 2" '("hel↑lo world.¶line 2" :killed "hello world.¶line 2")))
   ;; Prose units
   ((e:word (:forward ; :backward
             ))
    ("↑"                    'c:end-of-buffer)
    ("hel↑lo world.¶line 2" '("hel↑lo world.¶line 2" :killed "lo"))
    ("hello↑ world.¶line 2" '("hello↑ world.¶line 2" :killed "world")))
   ((e:sentence (:forward ; :backward
                 ))
    ("hello↑ world.¶line 2" '("hello↑ world.¶line 2" :killed " world."))
    ("hello world.↑¶line 2" '("hello world.↑¶line 2" :killed "line 2")))
   ((e:paragraph (:forward ; :backward
                  ))
    ("hel↑lo¶world.¶¶line 2" '("hel↑lo¶world.¶¶line 2" :killed "lo¶world.¶¶"))
    ("hello¶world.↑¶¶line 2" '("hello¶world.↑¶¶line 2" :killed "¶¶")))))

(test delete.insertion-stack
  "Test captured insertions for the `delete' operation."
  (operation-cases (e:delete :target :buffer)
   ;; Buffer structure units
   ((e:item (:forward :backward))
    ("hel↑lo world.¶line 2" '("hel↑o world.¶line 2" :killed "l")
                            '("he↑lo world.¶line 2" :killed "l")))
   ((e:line (:forward :backward))
    ("↑hello world.¶line 2" `("↑line 2"  :killed ,(format nil "hello world.~%"))
                            '("↑¶line 2" :killed "hello world.")))
   ((e:semi-line (:forward :backward))
    ("hello↑ world.¶line 2" '("hello↑¶line 2"   :killed " world.")
                            '("↑ world.¶line 2" :killed "hello")))
   ((e:buffer (:forward :backward))
    ("hel↑lo world.¶line 2" '("↑" :killed "hello world.¶line 2")
                            '("↑" :killed "hello world.¶line 2")))
   ((e:semi-buffer (:forward :backward))
    ("hel↑lo world.¶line 2" '("hel↑"              :killed "lo world.¶line 2")
                            '("↑lo world.¶line 2" :killed "hel")))
   ;; Prose units
   ((e:word (:forward :backward))
    ("hel↑lo world.¶line 2" '("hel↑ world.¶line 2" :killed "lo")
                            '("↑lo world.¶line 2"  :killed "hel"))
    ("hello↑ world.¶line 2" '("hello↑.¶line 2"     :killed " world")
                            '("↑ world.¶line 2"    :killed "hello")))
   ((e:sentence (:forward :backward))
    ("hello↑ world.¶line 2" '("hello↑¶line 2"   :killed " world.")
                            '("↑ world.¶line 2" :killed "hello"))
    ("hello world.↑¶line 2" '("hello world.↑"   :killed "¶line 2")
                            '("↑¶line 2"        :killed "hello world.")))
   ((e:paragraph (:forward :backward))
    ("hel↑lo¶world.¶¶line 2" '("hel↑line 2"          :killed "lo¶world.¶¶")
                             '("↑lo¶world.¶¶line 2"  :killed "hel"))
    ("hello¶world.↑¶¶line 2" '("hello¶world.↑line 2" :killed "¶¶")
                             '("↑¶¶line 2"           :killed "hello¶world.")))))

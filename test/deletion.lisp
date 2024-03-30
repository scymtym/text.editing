;;;; deletion.lisp --- Tests for deletion operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.deletion
  :in :text.editing)

(test delete.smoke
  "Smoke test for the `delete' operation."
  (operation-cases (e:delete)
   ;; Buffer structure units
   ((e:item (:forward :backward))
    ("↑hello world.¶line 2" "↑ello world.¶line 2" 'c:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hel↑o world.¶line 2" "he↑lo world.¶line 2")
    ("hello↑ world.¶line 2" "hello↑world.¶line 2" "hell↑ world.¶line 2")
    ("hello world.↑¶line 2" "hello world.↑line 2" "hello world↑¶line 2")
    ("hello world.¶↑line 2" "hello world.¶↑ine 2" "hello world.↑line 2")
    ("hello world.¶line 2↑" 'c:end-of-buffer      "hello world.¶line ↑"))
   ((e:line (:forward :backward))
    ("↑hello world.¶line 2" "↑line 2"        "↑¶line 2")
    ("hel↑lo world.¶line 2" "↑line 2"        "↑¶line 2")
    ("hello↑ world.¶line 2" "↑line 2"        "↑¶line 2")
    ("hello world.↑¶line 2" "↑line 2"        "↑¶line 2")
    ("hello world.¶↑line 2" "hello world.¶↑" "hello world.↑")
    ("hello world.¶line 2↑" "hello world.¶↑" "hello world.↑"))
   ((e:semi-line (:forward :backward))
    ("↑hello world.¶line 2" "↑¶line 2"            'c:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hel↑¶line 2"         "↑lo world.¶line 2")
    ("hello↑ world.¶line 2" "hello↑¶line 2"       "↑ world.¶line 2")
    ("hello world.↑¶line 2" "hello world.↑line 2" "↑¶line 2")  ; specialized `delete' method handles this
    ("hello world.¶↑line 2" "hello world.¶↑"      "hello world.↑line 2")
    ("hello world.¶line 2↑" 'c:end-of-buffer      "hello world.¶↑"))
   ;; TODO line-boundary
   ;; (delete-case "hello world↑"         "↑hello world" e:line-boundary :forward)
   ;; (delete-case "hello world↑"         "hel↑lo world" e:line-boundary :forward)
   ;; (delete-case "hello world↑"         "hello↑ world" e:line-boundary :forward)
   ;; (delete-case 'c:end-of-buffer       "hello world↑" e:line-boundary :forward)
   ;; (delete-case 'c:beginning-of-buffer "↑hello world" e:line-boundary :backward)
   ;; (delete-case "↑hello world"         "hel↑lo world" e:line-boundary :backward)
   ;; (delete-case "↑hello world"         "hello world↑" e:line-boundary :backward)
   ;; (delete-case "↑hello world"         "hello↑ world" e:line-boundary :backward)
   ((e:buffer (:forward :backward))
    ("↑hello world.¶line 2" (:always "↑"))
    ("hel↑lo world.¶line 2" (:always "↑"))
    ("hello↑ world.¶line 2" (:always "↑"))
    ("hello world.↑¶line 2" (:always "↑"))
    ("hello world.¶↑line 2" (:always "↑"))
    ("hello world.¶line 2↑" (:always "↑")))
   ((e:semi-buffer (:forward :backward))
    ("↑hello world.¶line 2" "↑"                    "↑hello world.¶line 2")
    ("hel↑lo world.¶line 2" "hel↑"                 "↑lo world.¶line 2")
    ("hello↑ world.¶line 2" "hello↑"               "↑ world.¶line 2")
    ("hello world.↑¶line 2" "hello world.↑"        "↑¶line 2")
    ("hello world.¶↑line 2" "hello world.¶↑"       "↑line 2")
    ("hello world.¶line 2↑" "hello world.¶line 2↑" "↑"))
   ;; TODO buffer-boundary
   ;; Prose units
   ((e:word (:forward :backward))
    ("↑hello world.¶line 2" "↑ world.¶line 2"    'c:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hel↑ world.¶line 2" "↑lo world.¶line 2")
    ("hello↑ world.¶line 2" "hello↑.¶line 2"     "↑ world.¶line 2")
    ("hello world.↑¶line 2" "hello world.↑ 2"    "hello ↑¶line 2")
    ("hello world.¶↑line 2" "hello world.¶↑ 2"   "hello ↑line 2")
    ("hello world.¶line 2↑" 'c:end-of-buffer     "hello world.¶line ↑"))
   ((e:sentence (:forward :backward))
    ("↑hello world.¶line 2" "↑¶line 2"       'c:beginning-of-buffer)
    ("hel↑lo world.¶line 2" "hel↑¶line 2"    "↑lo world.¶line 2")
    ("hello↑ world.¶line 2" "hello↑¶line 2"  "↑ world.¶line 2")
    ("hello world.↑¶line 2" "hello world.↑"  "↑¶line 2")
    ("hello world.¶↑line 2" "hello world.¶↑" "↑line 2")
    ("hello world.¶line 2↑" 'c:end-of-buffer "hello world.¶↑"))
   ((e:paragraph (:forward :backward))
    ("↑hello¶world.¶¶line 2" "↑line 2"             'c:beginning-of-buffer)
    ("hel↑lo¶world.¶¶line 2" "hel↑line 2"          "↑lo¶world.¶¶line 2")
    ("hello↑¶world.¶¶line 2" "hello↑line 2"        "↑¶world.¶¶line 2")
    ("hello¶world.↑¶¶line 2" "hello¶world.↑line 2" "↑¶¶line 2")
    ("hello¶world.¶¶↑line 2" "hello¶world.¶¶↑"     "↑line 2")
    ("hello¶world.¶¶line 2↑" 'c:end-of-buffer      "hello¶world.¶¶↑"))))

(test delete.region
  "Smoke test for applying the `delete' operation to regions."
  ;; This is a separate test because it must use :target :buffer
  ;; instead of :target :point so that both the point and the mark
  ;; cursor are available.
  (operation-cases (e:delete :target :buffer)
   ((e:region (:forward #+later :backward))
    ("↑hello world"  'e:mark-not-set-error                  #+later 'e:mark-not-set-error)
    ("↑hello world↓" 'e:mark-not-active-error               #+later 'e:mark-not-active-error)
    ("↑hello world↧" '("↧↑"          :killed "hello world") #+later '("↑↧" :killed "hello world"))
    ("↑hello ↧world" '("↧↑world"     :killed "hello ")      #+later '("↑↧" :killed "hello world"))
    ("hello ↧wo↑rld" '("hello ↧↑rld" :killed "wo")          #+later '("↑↧" :killed "hello world")))
   ((e:region-or-item (:forward #+later :backward))
    ("h↑ello world"  '("h↑llo world"  :killed "e")           #+later '("↑ello world"  :killed "h"))
    ("hello w↑orld↓" '("hello w↑rld↓" :killed "o")           #+later '("hello ↑orld↓" :killed "w"))
    ("↑hello world↧" '("↧↑"           :killed "hello world") #+later '("↑↧"           :killed "hello world"))
    ("↑hello ↧world" '("↧↑world"      :killed "hello ")      #+later '("↑↧"           :killed "hello world"))
    ("hello ↧wo↑rld" '("hello ↧↑rld"  :killed "wo")          #+later '("↑↧"           :killed "hello world")))))

(test delete-indentation.smoke
  "Smoke test for the `delete-indentation' operation."
  (operation-cases (e:delete-indentation)
   (()
    ("foo↑ bar"      "↑foo bar")
    ("¶↑foo bar"     "↑foo bar")
    ("¶foo↑ bar"     "↑foo bar")
    ("¶foo bar↑"     "↑foo bar")
    ("foo¶↑     bar" "foo↑ bar")
    ("foo¶     ↑bar" "foo↑ bar")
    ("foo¶     bar↑" "foo↑ bar")
    ("foo  ¶↑   bar" "foo↑ bar")
    ("foo  ¶   ↑bar" "foo↑ bar")
    ("foo  ¶   bar↑" "foo↑ bar"))))

(test delete-trailing-whitespace.smoke
  "Smoke test for the `delete-trailing-whitespace' operation."
  (operation-cases (e:delete-trailing-whitespace)
   (()
    ("↑"                   "↑")
    ("↑foo"                "↑foo")
    ("foo↑"                "foo↑")
    ("foo       ↑        " "foo↑"))))

(test fixup-whitespace.smoke
  "Smoke test for the `fixup-whitespace' operation."
  (operation-cases (e:fixup-whitespace)
   (()
    ("↑"             "↑")
    ("↑foo"          "↑foo")
    ("↑   foo"       "↑foo")
    ("  ↑ foo"       "↑foo")
    ("   ↑foo"       "↑foo")
    ("foo↑bar"       "foo↑ bar")
    ("foo      ↑bar" "foo↑ bar")
    ("foo↑    bar"   "foo↑ bar")
    ("foo  ↑  bar"   "foo↑ bar"))))

;;;; items.lisp --- Tests for ITEMS functions.
;;;;
;;;; Copyright (C) 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.items
  :in :text.editing)

(test items.smoke
  "Smoke test for the `items' function."
  (operation-cases (e:items :target :buffer)
   ;; Region
   ((e:region :forward)
    ("↑foo"  'e:mark-not-set-error)
    ("↑foo↓" 'e:mark-not-active-error)
    ("↑foo↧" '(:value "foo"))
    ("↧foo↑" '(:value "foo")))
   ;; Buffer structure units
   ((e:item :forward)
    ("↑foo" '(:value "f"))
    ("foo↑" 'c:end-of-buffer))
   ((e:line :forward)
    ("↑foo bar¶baz" `(:value ,(format nil "foo bar~%")))
    ("foo↑ bar¶baz" `(:value ,(format nil "foo bar~%")))
    ("foo bar↑¶baz" `(:value ,(format nil "foo bar~%")))
    ("foo bar¶↑baz" '(:value "baz"))
    ("foo bar¶baz↑" '(:value "baz")))
   ((e:semi-line :forward)
    ("↑foo bar¶baz" '(:value "foo bar"))
    ("foo↑ bar¶baz" '(:value " bar"))
    ("foo bar↑¶baz" '(:value ""))
    ("foo bar¶↑baz" '(:value "baz"))
    ("foo bar¶baz↑" '(:value "")))
   ((e:buffer :forward)
    ("↑foo" '(:value "foo"))
    ("foo↑" '(:value "foo")))
   ;; Prose units
   ((e:word :forward)
    ("↑foo" '(:value "foo"))
    ("foo↑" 'c:end-of-buffer))))

(test setf-items.smoke
  "Smoke test for the (setf items) function."
  (operation-cases ((setf e:items) :target :buffer)
   ;; Region
   ((("bar" '(#\b #\a #\r)) e:region :forward)
    ("↑foo"  (:always 'e:mark-not-set-error))
    ("↑foo↓" (:always 'e:mark-not-active-error))
    ("↑foo↧" "↧bar↑" "↧bar↑")
    ("↧foo↑" "↧bar↑" "↧bar↑"))
   ;; Buffer structure units
   ((("bar" '(#\b #\a #\r)) e:item (:forward :backward))
    ("↑foo" "bar↑oo"         'c:beginning-of-buffer
            "bar↑oo"         'c:beginning-of-buffer)
    ("foo↑" 'c:end-of-buffer "fobar↑"
            'c:end-of-buffer "fobar↑"))
   ((("bar" '(#\b #\a #\r)) e:line (:forward :backward))
    ("↑foo" "bar↑" "bar↑" "bar↑" "bar↑")
    ("foo↑" "bar↑" "bar↑" "bar↑" "bar↑"))
   ((("bar" '(#\b #\a #\r)) e:buffer (:forward :backward))
    ("↑foo" "bar↑" "bar↑" "bar↑" "bar↑")
    ("foo↑" "bar↑" "bar↑" "bar↑" "bar↑"))
   ;; Prose units
   ((("bar" '(#\b #\a #\r)) e:word (:forward :backward))
    ("↑foo" "bar↑"           'c:beginning-of-buffer
            "bar↑"           'c:beginning-of-buffer)
    ("foo↑" 'c:end-of-buffer "bar↑"
            'c:end-of-buffer "bar↑"))))

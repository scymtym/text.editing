;;;; delimiter-pairs.lisp --- Tests for delimiter pair operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.delimiter-pairs
  :in :text.editing)

(test insert-opening-and-closing-delimiter.smoke
  "Smoke test for the `insert-opening-and-closing-delimiter' function."
  (operation-cases (e::insert-opening-and-closing-delimiter)
    ((#\( #\))
     ("↑"   "(↑)")
     ("↑ "   "(↑) ")
     ("↑1"  "(↑) 1")
     ("↑¶1" "(↑)¶1"))
    ((#\" #\")
     ("↑"   "\"↑\"")
     ("↑ "  "\"↑\" ")
     ("↑1"  "\"↑\" 1")
     ("↑¶1" "\"↑\"¶1"))))

(test insert-delimiter-pair.smoke
  "Smoke test for the `insert-delimiter-pair' function."
  (operation-cases (e:insert-delimiter-pair)
    ((#\()
     ("↑"   "(↑)")
     ("↑1"  "(↑) 1")
     ("↑¶1" "(↑)¶1"))
    ((#\")
     ("↑"   "\"↑\"")
     ("↑1"  "\"↑\" 1")
     ("↑¶1" "\"↑\"¶1"))))

(test maybe-move-past-closing-delimiter.smoke
  "Smoke test for the `maybe-move-past-closing-delimiter' function."
  (operation-cases (e:maybe-move-past-closing-delimiter)
    ((#\) :whitespace (nil :move-past :delete))
     ("↑"     '(:value nil :state "↑")     '(:value nil :state "↑")     '(:value nil :state "↑"))
     ("↑¶1"   '(:value nil :state "↑¶1")   '(:value nil :state "↑¶1")   '(:value nil :state "↑¶1"))
     ("↑1"    '(:value nil :state "↑1")    '(:value nil :state "↑1")    '(:value nil :state "↑1"))
     ("↑)"    '(:value t   :state ")↑")    '(:value t   :state ")↑")    '(:value t   :state ")↑"))
     ("↑  )"  '(:value nil :state "↑  )")  '(:value t   :state "  )↑")  '(:value t   :state ")↑"))
     ("↑¶  )" '(:value nil :state "↑¶  )") '(:value t   :state "¶  )↑") '(:value t   :state ")↑")))
    ((#\" :whitespace (nil :move-past :delete))
     ("↑"      '(:value nil :state "↑")      '(:value nil :state "↑")      '(:value nil :state "↑"))
     ("↑¶1"    '(:value nil :state "↑¶1")    '(:value nil :state "↑¶1")    '(:value nil :state "↑¶1"))
     ("↑1"     '(:value nil :state "↑1")     '(:value nil :state "↑1")     '(:value nil :state "↑1"))
     ("↑\""    '(:value t   :state "\"↑")    '(:value t   :state "\"↑")    '(:value t   :state "\"↑"))
     ("↑  \""  '(:value nil :state "↑  \"")  '(:value t   :state "  \"↑")  '(:value t   :state "\"↑"))
     ("↑¶  \"" '(:value nil :state "↑¶  \"") '(:value t   :state "¶  \"↑") '(:value t   :state "\"↑")))))

(test move-past-closing-delimiter.smoke
  "Smoke test for the `move-past-closing-delimiter' function."
  (operation-cases (e:move-past-closing-delimiter)
    ((#\|)
     ("↑"     'e:no-closing-delimiter-error)
     ("↑¶1"   'e:no-closing-delimiter-error)
     ("↑1"    'e:no-closing-delimiter-error)
     ("↑|"    "|↑")
     ("↑  |"  "|↑")
     ("↑¶  |" "|↑"))
    ((#\")
     ("↑"      'e:no-closing-delimiter-error)
     ("↑¶1"    'e:no-closing-delimiter-error)
     ("↑1"     'e:no-closing-delimiter-error)
     ("↑\""    "\"↑")
     ("↑  \""  "\"↑")
     ("↑¶  \"" "\"↑"))))

(test move-past-closing-delimiter-or-insert-delimiter-pair.smoke
  "Smoke test for the `move-past-closing-delimiter-or-insert-delimiter-pair' function."
  (operation-cases (e:move-past-closing-delimiter-or-insert-delimiter-pair)
    ((#\|)
     ("↑"     '(:value t   :state "|↑|"))
     ("↑¶1"   '(:value t   :state "|↑|¶1"))
     ("↑1"    '(:value t   :state "|↑| 1"))
     ("↑|"    '(:value nil :state "|↑"))
     ("↑  |"  '(:value t   :state "|↑|  |"))
     ("↑¶  |" '(:value t   :state "|↑|¶  |")))
    ((#\")
     ("↑"      '(:value t   :state "\"↑\""))
     ("↑¶1"    '(:value t   :state "\"↑\"¶1"))
     ("↑1"     '(:value t   :state "\"↑\" 1"))
     ("↑\""    '(:value nil :state "\"↑"))
     ("↑  \""  '(:value t   :state "\"↑\"  \""))
     ("↑¶  \"" '(:value t   :state "\"↑\"¶  \"")))))

(test delete-delimiter-pair-or-item.smoke
  "Smoke test for the `delete-delimiter-pair-or-item' function."
  (operation-cases (e:delete-delimiter-pair-or-item)
    (((:forward :backward) :if-not-empty (nil :move-past :delete-inside))
     ;; No delimiters
     ("↑"      'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
               'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("↑1"     "↑"                    "↑"                    "↑"
               'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("1↑"     'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
               "↑"                    "↑"                    "↑")
     ;; Heterogeneous delimiters
     ("↑()"    "↑"                    "↑"                    "↑"
               'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("↑( )"   "↑( )"                 "(↑ )"                 "↑()"
               'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("()↑"    'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
               "↑"                    "↑"                    "↑")
     ("( )↑"   'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
               "( )↑"                 "( ↑)"                 "()↑")
     ("(↑)"    "↑"                    "↑"                    "↑"
               "↑"                    "↑"                    "↑")
     ("(↑ )"   "(↑)"                  "(↑)"                  "(↑)"
               "(↑ )"                 "↑( )"                 "(↑)")
     ("( ↑)"   "( ↑)"                 "( )↑"                 "(↑)"
               "(↑)"                  "(↑)"                  "(↑)")
     ;; Homogeneous delimiters
     ;;
     ;; This does not work so well since we cannot determine whether a
     ;; given delimiter item is the opening or closing delimiter.
     ("↑\"\""  "↑"                    "↑"                    "↑"
               'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     #+no ("↑\" \"" "↑\" \""               "\"↑ \""               "↑\"\""
               'c:beginning-of-buffer 'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("\"\"↑"  'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
               "↑"                    "↑"                    "↑")
     #+no ("\" \"↑" 'c:end-of-buffer       'c:end-of-buffer       'c:end-of-buffer
               "\" \"↑"               "\" ↑\""               "\"\"↑")
     ("\"↑\""  "↑"                    "↑"                    "↑"
               "↑"                    "↑"                    "↑")
     ("\"↑ \"" "\"↑\""                "\"↑\""                "\"↑\""
               "\"↑ \""               "↑\" \""               "\"↑\"")
     ("\" ↑\"" "\" ↑\""               "\" \"↑"               "\"↑\""
               "\"↑\""                "\"↑\""                "\"↑\""))))

(test surround-with-delimiter-pair.smoke
  "Smoke test for the `surround-with-delimiter-pair' function."
  (operation-cases (e:surround-with-delimiter-pair)
    (((e:word) (:forward :backward) #\( :count (1 2))
     ("↑"       'c:end-of-buffer       'c:end-of-buffer
                'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("↑1 2"    "(↑1) 2"               "(↑1 2)"
                'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("1 2↑"    'c:end-of-buffer       'c:end-of-buffer
                "1 (2↑)"               "(1 2↑)")
     ("↑ 1 2"   "(↑ 1) 2"              "(↑ 1 2)"
                'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("1 2 ↑"   'c:end-of-buffer       'c:end-of-buffer
                "1 (2 ↑)"              "(1 2 ↑)")
     ("↑(1) 2"  "(↑(1)) 2"             "(↑(1) 2)"
                'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("1 (2)↑"  'c:end-of-buffer       'c:end-of-buffer
                "1 ((2)↑)"             "(1 (2)↑)")
     ("↑ (1) 2" "(↑ (1)) 2"            "(↑ (1) 2)"
                'c:beginning-of-buffer 'c:beginning-of-buffer)
     ("1 (2) ↑" 'c:end-of-buffer       'c:end-of-buffer
                "1 ((2) ↑)"            "(1 (2) ↑)")
     ("(↑)"     "((↑))"                'c:end-of-buffer
                "((↑))"                'c:beginning-of-buffer))))

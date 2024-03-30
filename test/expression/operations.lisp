;;;; operations.lisp --- Test for operations involving expression units.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.expression.test)

(def-suite* :text.editing.expression.operations
  :in :text.editing)

(test move.smoke
  "Smoke test for the `move' operation with expression units."
  (operation-cases (e:move :buffer-class 'test-buffer)
   ((x:expression (:forward :backward))
    ("↑1"     "1↑"                                'c:beginning-of-buffer)
    ("1↑"     'c:end-of-buffer                    "↑1")
    (" ↑1"    " 1↑"                               'x:no-expression-before-cursor-error)
    ("1↑ "    'x:no-expression-after-cursor-error "↑1 ")

    ("↑foo"   "foo↑"                              'c:beginning-of-buffer)
    ("foo↑"   'c:end-of-buffer                    "↑foo")
    (" ↑foo"  " foo↑"                             'x:no-expression-before-cursor-error)
    ("foo↑ "  'x:no-expression-after-cursor-error "↑foo ")

    ("↑(1)"   "(1)↑"                              'c:beginning-of-buffer)
    ("(1)↑"   'c:end-of-buffer                    "↑(1)")
    (" ↑(1)"  " (1)↑"                             'x:no-expression-before-cursor-error)
    ("(1)↑ "  'x:no-expression-after-cursor-error "↑(1) ")

    ("(↑1)"   "(1↑)"                              "↑(1)")
    ("(1↑)"   "(1)↑"                              "(↑1)")
    ("(↑(1))" "((1)↑)"                            "↑((1))")
    ("((1)↑)" "((1))↑"                            "(↑(1))")
    ("((↑1))" "((1↑))"                            "(↑(1))")
    ("((1↑))" "((1)↑)"                            "((↑1))"))
   ((x:toplevel-expression (:forward :backward))
    ("↑1"     "1↑"                                'c:beginning-of-buffer)
    ("1↑"     'c:end-of-buffer                    "↑1")
    (" ↑1"    " 1↑"                               'x:no-expression-before-cursor-error)
    ("1↑ "    'x:no-expression-after-cursor-error "↑1 ")

    ("↑foo"   "foo↑"                              'c:beginning-of-buffer)
    ("foo↑"   'c:end-of-buffer                    "↑foo")
    (" ↑foo"  " foo↑"                             'x:no-expression-before-cursor-error)
    ("foo↑ "  'x:no-expression-after-cursor-error "↑foo ")

    ("↑(1)"   "(1)↑"                              'c:beginning-of-buffer)
    ("(1)↑"   'c:end-of-buffer                    "↑(1)")
    (" ↑(1)"  " (1)↑"                             'x:no-expression-before-cursor-error)
    ("(1)↑ "  'x:no-expression-after-cursor-error "↑(1) ")

    ("(↑1)"   "(1)↑"                              "↑(1)")
    ("(1↑)"   "(1)↑"                              "↑(1)")
    ("(↑(1))" "((1))↑"                            "↑((1))")
    ("((1)↑)" "((1))↑"                            "↑((1))")
    ("((↑1))" "((1))↑"                            "↑((1))")
    ("((1↑))" "((1))↑"                            "↑((1))"))))

(test delete.smoke
  "Smoke test for the `delete' operation with expression units."
  (operation-cases (e:delete :buffer-class 'test-buffer)
   ((x:expression (:forward :backward))
    ("↑1"    "↑"                                  'c:beginning-of-buffer)
    ("1↑"    'c:end-of-buffer                     "↑")
    (" ↑1"   " ↑"                                 'x:no-expression-before-cursor-error)
    ("1↑ "   'x:no-expression-after-cursor-error  "↑ ")

    ("↑foo"  "↑"                                  'c:beginning-of-buffer)
    ("foo↑"  'c:end-of-buffer                     "↑")
    (" ↑foo" " ↑"                                 'x:no-expression-before-cursor-error)
    ("foo↑ " 'x:no-expression-after-cursor-error  "↑ ")

    ("↑(1)"   "↑"                                 'c:beginning-of-buffer)
    ("(1)↑"   'c:end-of-buffer                    "↑")
    (" ↑(1)"  " ↑"                                'x:no-expression-before-cursor-error)
    ("(1)↑ "  'x:no-expression-after-cursor-error "↑ ")

    ("(↑1)"   "(↑)"                               "↑(1)") ; TODO this is an error in Emacs paredit
    ("(1↑)"   "(1)↑"                              "(↑)")
    ("(↑(1))" "(↑)"                               "↑((1))")
    ("((1)↑)" "((1))↑"                            "(↑)")
    ("((↑1))" "((↑))"                             "(↑(1))")
    ("((1↑))" "((1)↑)"                            "((↑))"))
   ((x:toplevel-expression (:forward :backward))
    ("↑1"     "↑"                                 'c:beginning-of-buffer)
    ("1↑"     'c:end-of-buffer                    "↑")

    ("↑foo"   "↑"                                 'c:beginning-of-buffer)
    ("foo↑"   'c:end-of-buffer                    "↑")
    (" ↑foo"  " ↑"                                'x:no-expression-before-cursor-error)
    ("foo↑ "  'x:no-expression-after-cursor-error "↑ ")

    ("↑(1)"   "↑"                                 'c:beginning-of-buffer)
    ("(1)↑"   'c:end-of-buffer                    "↑")
    (" ↑(1)"  " ↑"                                'x:no-expression-before-cursor-error)
    ("(1)↑ "  'x:no-expression-after-cursor-error "↑ ")

    ("(↑1)"   "↑"                                 "↑")
    ("(1↑)"   "↑"                                 "↑")
    ("(↑(1))" "↑"                                 "↑")
    ("((1)↑)" "↑"                                 "↑")
    ("((↑1))" "↑"                                 "↑")
    ("((1↑))" "↑"                                 "↑"))))

(test raise.smoke
  "Smoke test for the `raise' operation."
  (operation-cases (x:raise :buffer-class 'test-buffer)
   ((x:expression (:forward :backward))
    ;; Error cases
    ("↑"          (:always 'x:cursor-not-inside-expression-error))
    ("↑1"         'x:expression-at-toplevel-error       'x:cursor-not-inside-expression-error)
    ("1↑"         'x:cursor-not-inside-expression-error 'x:expression-at-toplevel-error)
    ("↑()"        'x:expression-at-toplevel-error       'x:cursor-not-inside-expression-error)
    ("()↑"        'x:cursor-not-inside-expression-error 'x:expression-at-toplevel-error)
    ;; Valid cases
    ("(↑1)"       "↑1"                                  'x:expression-at-toplevel-error)
    ("(1↑)"       'x:expression-at-toplevel-error       "1↑")
    ("(1 ↑)"      'x:expression-at-toplevel-error       "1↑")
    ("(1↑1)"      "1↑1"                                 "1↑1")
    ("(1 ↑ 2)"    "↑2"                                  "1↑")

    ("(1 ↑(2) 3)" "↑(2)"                                "1↑")
    ("(1 (↑2) 3)" "(1 ↑2 3)"                            "(↑2)")
    ("(1 (2↑) 3)" "(2↑)"                                "(1 2↑ 3)")
    ("(1 (2)↑ 3)" "↑3"                                  "(2)↑"))))

(test splice.smoke
  "Smoke test for the `splice' operation."
  (operation-cases (x:splice :buffer-class 'test-buffer)
   ((x:expression (nil :forward :backward))
    ;; Error cases
    ("↑"         (:always 'x:cursor-not-inside-expression-error))
    ("()↑"       (:always 'x:cursor-not-inside-expression-error))
    ("↑()"       (:always 'x:cursor-not-inside-expression-error))
    ;; Valid cases
    ("(↑)"       "↑"       "↑"      "↑")

    ;; ("(1↑1)"     "1↑1" "↑11" "11↑") ; TODO can't know to splice parent instead

    ("(↑ 1)"     "↑1"      "↑1"     "↑")
    ("(1 ↑)"     "1↑"      "↑"      "1↑")
    ("(1 ↑ 2)"   "1 ↑ 2"   "↑2"     "1↑")

    ("(↑ 1 2 3)" "↑1 2 3"  "↑1 2 3" "↑")
    ("(1 ↑ 2 3)" "1 ↑ 2 3" "↑2 3"   "1↑")
    ("(1 2 ↑ 3)" "1 2 ↑ 3" "↑3"     "1 2↑")
    ("(1 2 3 ↑)" "1 2 3↑"  "↑"      "1 2 3↑")

    ("\"↑\""     "↑"       "↑"      "↑")
    ("\"↑foo\""  "↑foo"    "↑foo"   "foo↑")
    ("\"f↑oo\""  "f↑oo"    "↑foo"   "foo↑")
    ("\"foo↑\""  "foo↑"    "↑foo"   "foo↑"))))

(test split.smoke
  "Smoke test for the `split' operation."
  (operation-cases (x:split :buffer-class 'test-buffer)
   ((x:expression)
    ;; Error cases
    ("↑"           'x:cursor-not-inside-expression-error)
    ("()↑"         'x:cursor-not-inside-expression-error)
    ("↑()"         'x:cursor-not-inside-expression-error)
    ;; Valid cases
    ("( ↑)"        "( )↑()")
    ("(1 ↑)"       "(1 )↑()")
    ("( ↑2)"       "( )↑(2)")

    ("(1 ↑2)"      "(1 )↑(2)")
    ("(1↑ 2)"      "(1)↑( 2)")
    ("(1 ↑ 2)"     "(1 )↑( 2)")

    ("#(1 ↑2)"     "#(1 )↑#(2)")
    ("#(1↑ 2)"     "#(1)↑#( 2)")
    ("#(1 ↑ 2)"    "#(1 )↑#( 2)")

    ("((1 ↑2))"    "((1 )↑(2))")
    ("((1↑ 2))"    "((1)↑( 2))")
    ("((1 ↑ 2))"   "((1 )↑( 2))")

    ("\"1 ↑2\""    "\"1 \"↑\"2\"")
    ("\"1 ↑ 2\""   "\"1 \"↑\" 2\"")
    ("(\"1 ↑2\")"  "(\"1 \"↑\"2\")")
    ("(\"1 ↑ 2\")" "(\"1 \"↑\" 2\")"))
   ((x:toplevel-expression)
    ;; Error cases
    ("↑"           'x:cursor-not-inside-expression-error)
    ("()↑"         'x:cursor-not-inside-expression-error)
    ("↑()"         'x:cursor-not-inside-expression-error)
    ;; Valid cases
    ("( ↑)"        "( )¶¶↑()")
    ("(1 ↑)"       "(1 )¶¶↑()")
    ("( ↑2)"       "( )¶¶↑(2)")

    ("(1 ↑2)"      "(1 )¶¶↑(2)")
    ("(1↑ 2)"      "(1)¶¶↑( 2)")
    ("(1 ↑ 2)"     "(1 )¶¶↑( 2)")

    ("#(1 ↑2)"     "#(1 )¶¶↑#(2)")
    ("#(1↑ 2)"     "#(1)¶¶↑#( 2)")
    ("#(1 ↑ 2)"    "#(1 )¶¶↑#( 2)")

    ("((1 ↑2))"    "((1 ))¶¶↑((2))")
    ("((1↑ 2))"    "((1))¶¶↑(( 2))")
    ("((1 ↑ 2))"   "((1 ))¶¶↑(( 2))")

    ("\"1 ↑2\""    "\"1 \"¶¶↑\"2\"")
    ("\"1 ↑ 2\""   "\"1 \"¶¶↑\" 2\"")
    ("(\"1 ↑2\")"  "(\"1 \")¶¶↑(\"2\")")
    ("(\"1 ↑ 2\")" "(\"1 \")¶¶↑(\" 2\")"))))

(test join.smoke
  "Smoke test for the `join' operation."
  (operation-cases (x:join :buffer-class 'test-buffer)
   ((x:expression)
    ;; Error cases
    ("↑"               'x:no-expression-before-cursor-error)
    ("()↑"             'x:no-expression-after-cursor-error)
    ("↑()"             'x:no-expression-before-cursor-error)
    ;; Valid cases
    ("()↑()"           "( ↑)")
    ("(1)↑()"          "(1 ↑)")
    ("()↑(2)"          "( ↑2)")

    ("(1)↑(2)"         "(1 ↑2)")
    ("(1)↑ (2)"        "(1↑ 2)")
    ("(1) ↑(2)"        "(1 ↑2)")
    ("(1) ↑ (2)"       "(1 ↑ 2)")

    ("#(1)↑(2)"        "#(1 ↑2)")
    ("#(1)↑ (2)"       "#(1↑ 2)")
    ("#(1) ↑(2)"       "#(1 ↑2)")
    ("#(1) ↑ (2)"      "#(1 ↑ 2)")

    ("((1)↑(2))"       "((1 ↑2))")
    ("((1)↑ (2))"      "((1↑ 2))")
    ("((1) ↑(2))"      "((1 ↑2))")
    ("((1) ↑ (2))"     "((1 ↑ 2))")

    ("\"1\"↑\"2\""     "\"1 ↑2\"")
    ("\"1\" ↑ \"2\""   "\"1 ↑ 2\"")
    ("(\"1\"↑\"2\")"   "(\"1 ↑2\")")
    ("(\"1\" ↑ \"2\")" "(\"1 ↑ 2\")"))))

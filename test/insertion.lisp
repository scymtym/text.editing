;;;; insertion.lisp --- Tests for insertion operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.insertion
  :in :text.editing)

(test insert-items.smoke
  "Smoke test for the `insert-items' operation."
  (flet ((test-case (expected input &rest arguments)
           (with-buffer (buffer input)
             (let* ((site   (e:site buffer))
                    (cursor (if (e:mark-active-p site)
                                (e:mark site)
                                (e:point site))))
               (apply #'e:insert-items cursor arguments))
             (is-buffer-state* expected buffer input 'e:insert-items arguments))))
    ;; Test sequence types with `:start' and `:end' arguments
    (test-case "foobar↑"  "↑"  "foobar")
    (test-case "↧foobar↑" "↑↧" "foobar")
    (test-case "bar↑"     "↑"  "foobar" :start 3)
    (test-case "↧bar↑"    "↑↧" "foobar" :start 3)
    (test-case "foo↑"     "↑"  "foobar" :end 3)
    (test-case "↧foo↑"    "↑↧" "foobar" :end 3)
    (test-case "ooba↑"    "↑"  "foobar" :start 1 :end 5)
    (test-case "↧ooba↑"   "↑↧" "foobar" :start 1 :end 5)
    (test-case "↑"        "↑"  "foobar" :start 3 :end 3)
    (test-case "↧↑"       "↑↧" "foobar" :start 3 :end 3)

    (test-case "foobar↑"  "↑"  #(#\f #\o #\o #\b #\a #\r))
    (test-case "↧foobar↑" "↑↧" #(#\f #\o #\o #\b #\a #\r))
    (test-case "bar↑"     "↑"  #(#\f #\o #\o #\b #\a #\r) :start 3)
    (test-case "↧bar↑"    "↑↧" #(#\f #\o #\o #\b #\a #\r) :start 3)
    (test-case "foo↑"     "↑"  #(#\f #\o #\o #\b #\a #\r) :end 3)
    (test-case "↧foo↑"    "↑↧" #(#\f #\o #\o #\b #\a #\r) :end 3)
    (test-case "ooba↑"    "↑"  #(#\f #\o #\o #\b #\a #\r) :start 1 :end 5)
    (test-case "↧ooba↑"   "↑↧" #(#\f #\o #\o #\b #\a #\r) :start 1 :end 5)
    (test-case "↑"        "↑"  #(#\f #\o #\o #\b #\a #\r) :start 3 :end 3)
    (test-case "↧↑"       "↑↧" #(#\f #\o #\o #\b #\a #\r) :start 3 :end 3)

    (test-case "foobar↑"  "↑"  '(#\f #\o #\o #\b #\a #\r))
    (test-case "↧foobar↑" "↑↧" '(#\f #\o #\o #\b #\a #\r))
    (test-case "bar↑"     "↑"  '(#\f #\o #\o #\b #\a #\r) :start 3)
    (test-case "↧bar↑"    "↑↧" '(#\f #\o #\o #\b #\a #\r) :start 3)
    (test-case "foo↑"     "↑"  '(#\f #\o #\o #\b #\a #\r) :end 3)
    (test-case "↧foo↑"    "↑↧" '(#\f #\o #\o #\b #\a #\r) :end 3)
    (test-case "ooba↑"    "↑"  '(#\f #\o #\o #\b #\a #\r) :start 1 :end 5)
    (test-case "↧ooba↑"   "↑↧" '(#\f #\o #\o #\b #\a #\r) :start 1 :end 5)
    (test-case "↑"        "↑"  '(#\f #\o #\o #\b #\a #\r) :start 3 :end 3)
    (test-case "↧↑"       "↑↧" '(#\f #\o #\o #\b #\a #\r) :start 3 :end 3)
    ;; Test different initial buffer contents
    (test-case "foobar↑"     "foo↑"     "bar")
    (test-case "f↑oo↧bar"    "f↑oo↧"    "bar")
    (test-case "bar↑foo"     "↑foo"     "bar")
    (test-case "↧barfoo↑"    "↧foo↑"    "bar")
    (test-case "foobar↑baz"  "foo↑baz"  "bar")
    (test-case "foo↧bar↑baz" "foo↑↧baz" "bar")))

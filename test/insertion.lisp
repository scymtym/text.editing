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
             (apply #'e:insert-items (e:point buffer) arguments)
             (is-buffer-state* expected buffer input 'e:insert-items arguments))))
    ;; Test sequence types with `:start' and `:end' arguments
    (test-case "foobar↑" "↑" "foobar")
    (test-case "bar↑"    "↑" "foobar" :start 3)
    (test-case "foo↑"    "↑" "foobar" :end 3)
    (test-case "ooba↑"   "↑" "foobar" :start 1 :end 5)

    (test-case "foobar↑" "↑" #(#\f #\o #\o #\b #\a #\r))
    (test-case "bar↑"    "↑" #(#\f #\o #\o #\b #\a #\r) :start 3)
    (test-case "foo↑"    "↑" #(#\f #\o #\o #\b #\a #\r) :end 3)
    (test-case "ooba↑"   "↑" #(#\f #\o #\o #\b #\a #\r) :start 1 :end 5)

    (test-case "foobar↑" "↑" '(#\f #\o #\o #\b #\a #\r))
    (test-case "bar↑"    "↑" '(#\f #\o #\o #\b #\a #\r) :start 3)
    (test-case "foo↑"    "↑" '(#\f #\o #\o #\b #\a #\r) :end 3)
    (test-case "ooba↑"   "↑" '(#\f #\o #\o #\b #\a #\r) :start 1 :end 5)
    ;; Test different initial buffer content
    (test-case "foobar↑"    "foo↑" "bar")
    (test-case "bar↑foo"    "↑foo" "bar")
    (test-case "foobar↑baz" "foo↑baz" "bar")))

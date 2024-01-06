;;;; filling.lisp --- Tests for filling operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

(def-suite* :text.editing.filling
  :in :text.editing)

(test fill-words.smoke
  "Smoke test for the `fill-words' operation."
  (flet ((test-case (expected-content* old-content words &rest fill-args)
           (with-buffer (buffer old-content)
             (let ((point (e:point buffer))
                   (mark  (e:mark buffer)))
               (apply #'e:fill-words point mark words fill-args)
               (is-buffer-state* expected-content* buffer
                                 old-content 'e:fill-words fill-args)))))
    ;; Empty input
    (test-case "↑foo a bar baz fez↓"
               "↑↓"
               '("foo" "a" "bar" "baz" "fez")
               :fill-column 80)
    ;; Non-empty input
    (test-case "↑foo a bar baz fez↓"
               "↑hello world↓"
               '("foo" "a" "bar" "baz" "fez")
               :fill-column 80)
    ;; Non-empty input
    (test-case "keep ↑foo a bar baz fez↓ this"
               "keep ↑hello world↓ this"
               '("foo" "a" "bar" "baz" "fez")
               :fill-column 80)
    ;; Smaller fill column
    (test-case "↑foo a bar¶baz fez↓"
               "↑hello world↓"
               '("foo" "a" "bar" "baz" "fez")
               :fill-column 10)
    (test-case "keep ↑foo a¶bar baz¶fez↓ this"
               "keep ↑hello world↓ this"
               '("foo" "a" "bar" "baz" "fez")
               :fill-column 10)
    ;; Prefix
    (test-case "|↑foo a bar¶|baz fez↓"
               "↑hello world↓"
               '("foo" "a" "bar" "baz" "fez")
               :fill-column 10 :prefix "|" :per-line-prefix "|")
    ;; Block comment
    (test-case "#|↑foo a¶  bar baz¶  fez|#↓"
               "↑hello world↓"
               '("foo" "a" "bar" "baz" "fez")
               :fill-column 10 :prefix "#|" :per-line-prefix "  " :suffix "|#")))

;;;; operations.lisp --- Tests for operations provided by the search module.
;;;;
;;;; Copyright (C) 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.search.test)

(def-suite* :text.editing.search.operations
  :in :text.editing.search)

(test incremental-search.invalid-operations
  "Test conditions signaled by attempting operations when the state is
not suitable."
  (with-buffer (buffer "" :buffer-class 'test-buffer)
    ;; Not in incremental search.
    (signals s:not-in-incremental-search-error
      (e:perform buffer 's:finish-incremental-search))
    (signals s:not-in-incremental-search-error
      (e:perform buffer 's:abort-incremental-search))
    (signals s:not-in-incremental-search-error
      (e:perform buffer 's:convert-matches-to-sites))
    (signals s:not-in-incremental-search-error
      (e:perform buffer 's:extend-query "foo"))
    (signals s:not-in-incremental-search-error
      (e:perform buffer 's:truncate-query))
    (signals s:not-in-incremental-search-error
      (e:perform buffer 's:next-match))
    (signals s:not-in-incremental-search-error
      (e:perform buffer 's:previous-match))
    ;; Already in incremental search.
    (e:perform buffer 's:incremental-search :forward)
    (signals s:already-in-incremental-search-error
      (e:perform buffer 's:incremental-search :forward))
    (e:perform buffer 's:abort-incremental-search)))

(test incremental-search.finish
  "Test finishing an incremental search normally."
  (let ((input "↑foo¶bar¶for¶ofoo"))
    (with-buffer (buffer input :buffer-class 'test-buffer)
      ;; Start incremental search.
      (let ((case-description (list input 's:incremental-search '(:forward))))
        (e:perform buffer 's:incremental-search :forward)
        (apply #'is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
               case-description))
      ;; Set initial query.
      (let ((case-description (list input 's:extend-query '("fo"))))
        (e:perform buffer 's:extend-query "fo")
        (apply #'is-matches-state
               '((0 0 0 2) (2 0 2 2) (3 1 3 3)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "fo↑o¶bar¶for¶ofoo" buffer
               case-description))
      ;; Extend query, removing one match.
      (let ((case-description (list input 's:extend-query '(#\o))))
        (e:perform buffer 's:extend-query #\o)
        (apply #'is-matches-state
               '((0 0 0 3) (3 1 3 4)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer
               case-description))
      ;; Truncate query, regaining the third match.
      (let ((case-description (list input 's:truncate-query)))
        (e:perform buffer 's:truncate-query)
        (apply #'is-matches-state
               '((0 0 0 2) (2 0 2 2) (3 1 3 3)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "fo↑o¶bar¶for¶ofoo" buffer
               case-description))
      ;; Switch to third match
      (let ((case-description (list input 's:next-match)))
        (e:perform buffer 's:next-match)
        (apply #'is-buffer-state* "foo¶bar¶fo↑r¶ofoo" buffer
               case-description))
      (let ((case-description (list input 's:next-match)))
        (e:perform buffer 's:next-match)
        (apply #'is-buffer-state* "foo¶bar¶for¶ofo↑o" buffer
               case-description))
      ;; Finish incremental search. Point cursor should remain at the
      ;; third match.
      (let ((case-description (list input 's:finish-incremental-search)))
        (e:perform buffer 's:finish-incremental-search)
        (is (null (s:search-state buffer)))
        (apply #'is-buffer-state* "foo¶bar¶for¶ofo↑o" buffer
               case-description)))))

(test incremental-search.abort
  "Test aborting an incremental search."
  (let ((input "↑foo¶bar¶for¶ofoo"))
    (with-buffer (buffer input :buffer-class 'test-buffer)
      ;; Start incremental search.
      (let ((case-description (list input 's:incremental-search '(:forward))))
        (e:perform buffer 's:incremental-search :forward)
        (apply #'is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
               case-description))
      ;; Set query.
      (let ((case-description (list input 's:extend-query '("foo"))))
        (e:perform buffer 's:extend-query "foo")
        (apply #'is-matches-state
               '((0 0 0 3) (3 1 3 4)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer
               case-description))
      ;; Next match.
      (let ((case-description (list input 's:next-match)))
        (e:perform buffer 's:next-match)
        (apply #'is-buffer-state* "foo¶bar¶for¶ofoo↑" buffer
               case-description))
      ;; Abort incremental search. Point cursor should return to
      ;; original location.
      (let ((case-description (list input 's:abort-incremental-search)))
        (e:perform buffer 's:abort-incremental-search)
        (is (null (s:search-state buffer)))
        (apply #'is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
               case-description)))))

(test incremental-search.convert-matches-to-sites
  "Test converting matches to sites."
  (let ((input "↑foo¶bar¶for¶ofoo"))
    (with-buffer (buffer input :buffer-class 'multiple-site-test-buffer)
      ;; Start incremental search.
      (let ((case-description (list input 's:incremental-search '(:forward))))
        (e:perform buffer 's:incremental-search :forward)
        (is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer case-description))
      ;; Set query.
      (let ((case-description (list input 's:extend-query '("foo"))))
        (e:perform buffer 's:extend-query "foo")
        (apply #'is-matches-state
               '((0 0 0 3) (3 1 3 4)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer
               case-description))
      ;; Convert matches to sites.
      (let ((case-description (list input 's:convert-matches-to-sites)))
        (e:perform buffer 's:convert-matches-to-sites)
        (is (null (s:search-state buffer)))
        (apply #'is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer
               case-description)
        (let ((other (e:other-sites buffer)))
          (is (= 1 (length other)))
          (apply #'text.editing.test::is-site-state*
                 "foo¶bar¶for¶ofoo↑" (first other) case-description))))))

(test incremental-search.next+previous-match
  "Test the `next-match' and `previous-match' operations."
  (let ((input "↑foo¶bar¶for¶ofoo"))
    (with-buffer (buffer input :buffer-class 'test-buffer)
      ;; Start incremental search.
      (e:perform buffer 's:incremental-search :forward)
      (is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
                        input 's:incremental-search '(:forward))
      ;; Set query.
      (let ((case-description (list input 's:extend-query '("foo"))))
        (e:perform buffer 's:extend-query "foo")
        (apply #'is-matches-state
               '((0 0 0 3) (3 1 3 4)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer
               case-description))
      ;; Next match. Possible without wrapping.
      (e:perform buffer 's:next-match)
      (is-buffer-state* "foo¶bar¶for¶ofoo↑" buffer input 's:next-match)
      ;; Next match. Signals error unless allowed to wrap around.
      (signals s:no-next-match-error
        (e:perform buffer 's:next-match :wrap-around nil))
      (e:perform buffer 's:next-match)
      (is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer input 's:next-match)
      ;; Previous match. Signals error unless allowed to wrap around.
      (signals s:no-previous-match-error
        (e:perform buffer 's:previous-match :wrap-around nil))
      (e:perform buffer 's:previous-match)
      (is-buffer-state* "foo¶bar¶for¶ofoo↑" buffer input 's:previous-match)
      ;; Abort.
      (e:perform buffer 's:abort-incremental-search)
      (is (null (s:search-state buffer)))
      (is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
                        input 's:finish-incremental-search))))

(test incremental-search.case-mode
  "Test the `case-mode' and (setf case-mode) operations."
  (let ((input "↑FOO¶bar¶foo¶ofoo"))
    (with-buffer (buffer input :buffer-class 'test-buffer)
      ;; Start incremental search.
      (e:perform buffer 's:incremental-search :forward)
      (is (eq :ignore (s:case-mode (s:search-state buffer))))
      ;; Set query.
      (let ((case-description (list input 's:extend-query '(#\o))))
        (e:perform buffer 's:extend-query "foo")
        (apply #'is-matches-state
               '((0 0 0 3) (2 0 2 3) (3 1 3 4)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "FOO↑¶bar¶foo¶ofoo" buffer
               case-description))
      ;; Change case mode.
      (let ((case-description (list input '(setf s:case-mode) '(:match))))
        (e:perform buffer '(setf s:case-mode) :match)
        (is (eq :match (s:case-mode (s:search-state buffer))))
        (apply #'is-matches-state
               '((2 0 2 3) (3 1 3 4)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "FOO¶bar¶foo↑¶ofoo" buffer
               case-description))
      ;; Change back case mode.
      (let ((case-description (list input '(setf s:case-mode) '(:ignore))))
        (e:perform buffer '(setf s:case-mode) :ignore)
        (is (eq :ignore (s:case-mode (s:search-state buffer))))
        (apply #'is-matches-state
               '((0 0 0 3) (2 0 2 3) (3 1 3 4)) (s:search-state buffer)
               case-description)
        (apply #'is-buffer-state* "FOO↑¶bar¶foo¶ofoo" buffer
               case-description))
      ;; Abort.
      (e:perform buffer 's:abort-incremental-search)
      (is (null (s:search-state buffer)))
      (is-buffer-state* "↑FOO¶bar¶foo¶ofoo" buffer
                        input 's:finish-incremental-search))))

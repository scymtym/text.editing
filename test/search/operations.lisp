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
      (e:perform buffer 's:incremental-search :forward)
      (is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
                        input 's:incremental-search '(:forward))
      ;; Set initial query.
      (e:perform buffer 's:extend-query "fo")
      (is-matches-state '((0 0 0 2) (2 0 2 2) (3 1 3 3))
                        (s:search-state buffer) input 's:extend-query)
      (is-buffer-state* "fo↑o¶bar¶for¶ofoo" buffer
                        input 's:extend-query '("foo"))
      ;; Extend query, removing one match.
      (e:perform buffer 's:extend-query #\o)
      (is-matches-state '((0 0 0 3) (3 1 3 4))
                        (s:search-state buffer) input 's:extend-query '(#\o))
      (is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer
                        input 's:extend-query '(#\o))
      ;; Truncate query, regaining the third match.
      (e:perform buffer 's:truncate-query)
      (is-matches-state '((0 0 0 2) (2 0 2 2) (3 1 3 3))
                        (s:search-state buffer) input 's:truncate-query)
      (is-buffer-state* "fo↑o¶bar¶for¶ofoo" buffer input 's:truncate-query)
      ;; Switch to third match
      (e:perform buffer 's:next-match)
      (is-buffer-state* "foo¶bar¶fo↑r¶ofoo" buffer input 's:next-match)
      (e:perform buffer 's:next-match)
      (is-buffer-state* "foo¶bar¶for¶ofo↑o" buffer input 's:next-match)
      ;; Finish incremental search. Point cursor should remain at the
      ;; third match.
      (e:perform buffer 's:finish-incremental-search)
      (is (null (s:search-state buffer)))
      (is-buffer-state* "foo¶bar¶for¶ofo↑o" buffer
                        input 's:finish-incremental-search))))

(test incremental-search.abort
  "Test aborting an incremental search."
  (let ((input "↑foo¶bar¶for¶ofoo"))
    (with-buffer (buffer input :buffer-class 'test-buffer)
      ;; Start incremental search.
      (e:perform buffer 's:incremental-search :forward)
      (is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
                        input 's:incremental-search '(:forward))
      ;; Set query.
      (e:perform buffer 's:extend-query "foo")
      (is-matches-state '((0 0 0 3) (3 1 3 4))
                        (s:search-state buffer) input 's:extend-query '("foo"))
      (is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer input 's:extend-query '("foo"))
      ;; Next match.
      (e:perform buffer 's:next-match)
      (is-buffer-state* "foo¶bar¶for¶ofoo↑" buffer input 's:next-match)
      ;; Abort incremental search. Point cursor should return to
      ;; original location.
      (e:perform buffer 's:abort-incremental-search)
      (is (null (s:search-state buffer)))
      (is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
                        input 's:abort-incremental-search))))

(test incremental-search.convert-matches-to-sites
  "Test converting matches into sites."
  (let ((input "↑foo¶bar¶for¶ofoo"))
    (with-buffer (buffer input :buffer-class 'multiple-site-test-buffer)
      (e:perform buffer 's:incremental-search :forward)
      (is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
                        input 's:incremental-search '(:forward))

      (e:perform buffer 's:extend-query "foo")
      (is-matches-state
       '((0 0 0 3) (3 1 3 4))
       (s:search-state buffer) input 's:extend-query '("foo"))
      (is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer input 's:extend-query)

      (e:perform buffer 's:convert-matches-to-sites)
      (is (null (s:search-state buffer)))
      (is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer
                        input 's:convert-matches-to-sites))))

(test incremental-search.next+previous-match
  "Test the `next-match' and `previous-match' operations."
  (let ((input "↑foo¶bar¶for¶ofoo"))
    (with-buffer (buffer input :buffer-class 'test-buffer)
      ;; Start incremental search.
      (e:perform buffer 's:incremental-search :forward)
      (is-buffer-state* "↑foo¶bar¶for¶ofoo" buffer
                        input 's:incremental-search '(:forward))
      ;; Set query.
      (e:perform buffer 's:extend-query "foo")
      (is-matches-state '((0 0 0 3) (3 1 3 4))
                        (s:search-state buffer) input 's:extend-query '(#\o))
      (is-buffer-state* "foo↑¶bar¶for¶ofoo" buffer
                        input 's:extend-query '(#\o))
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

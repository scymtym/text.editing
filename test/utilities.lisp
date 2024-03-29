;;;; utilities.lisp --- Utilities for tests of the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.test)

;;; Cursor utilities

(defun expand-newlines (string)
  (substitute #\Newline #\¶ string))

(defun parse-cursor-string (string &key (placeholder #\↑))
  (a:if-let ((position (position placeholder string)))
    (let* ((line-number   (count #\Newline string :end position))
           (line-start    (1+ (or (position #\Newline string
                                            :end position :from-end t)
                                  -1)))
           (column-number (- position line-start))
           (content       (remove placeholder string :count 1)))
      (values content line-number column-number))
    (values string nil nil)))

(defun parse-mark-state (string)
  (multiple-value-bind (content line-number column-number)
      (parse-cursor-string string :placeholder #\↧)
    (if (null line-number)
        (parse-cursor-string string :placeholder #\↓)
        (values content line-number column-number t))))

(defun parse-state-string (state-string)
  (let ((newline-string (expand-newlines state-string)))
    (parse-cursor-string newline-string)))

(defun parse-state (state-specifier)
  (typecase state-specifier
    (string
     (parse-state-string state-specifier))
    (cons
     (destructuring-bind (state-string &key (killed nil killed-supplied?))
         state-specifier
       (let ((killed (if killed-supplied?
                         (a:ensure-list killed)
                         '*)))
         (multiple-value-bind (content point-line-number point-column-number)
             (parse-state-string state-string)
           (values content point-line-number point-column-number killed)))))))

(defun parse-state-with-mark (state-specifier)
  (multiple-value-bind (content1 point-line-number point-column-number killed)
      (parse-state state-specifier)
    (unless (and point-line-number point-column-number)
      (error "~@<State string ~S does not specify point.~@:>"
             state-specifier))
    (multiple-value-bind (content
                          mark-line-number mark-column-number mark-active-p)
        (parse-mark-state content1)
      (when (and mark-line-number
                 (= point-line-number mark-line-number)
                 (< mark-column-number point-column-number))
        (decf point-column-number))
      (values content
              point-line-number point-column-number
              mark-line-number mark-column-number
              mark-active-p
              killed))))

;;; Buffer utilities

(defclass test-buffer (e:site-mixin
                       cluffer-standard-buffer:buffer)
  ()
  (:default-initargs
   :initial-line (make-instance 'cluffer-standard-line:open-line)))

(defun buffer-string (buffer)
  (let ((items (e:items (e:point buffer) e:buffer :forward)))
    (coerce items 'string)))

(defun (setf buffer-string) (new-value buffer)
  (let ((point (e:point buffer)))
    (prog1
        (setf (e::items point e:buffer :forward) new-value)
      (e:move point e:buffer-boundary :backward))))

;;; `with-buffer'

(defun check-leaked-cursors (buffer)
  (let* ((all-cursors (loop :for line-number :below (c:line-count buffer)
                            :for line = (c:find-line buffer line-number)
                            :appending (cluffer-standard-line::cursors
                                        line)))
         (leaked      all-cursors))
    (e:map-sites (lambda (site)
                   (setf leaked (remove (e:point site) leaked))
                   (a:when-let ((mark (e:mark site)))
                     (setf leaked (remove mark leaked)))
                   (setf leaked (set-difference leaked (e:mark-stack site))))
                 buffer)
    (is (null leaked)
        "~@<Expected no additional cursors to remain but found ~A~@:>"
        leaked)))

(defun call-with-buffer (function state-designator
                         &key (buffer-class 'test-buffer)
                              (point-line   0            point-line-supplied?)
                              (point-column 0            point-column-supplied?))
  (multiple-value-bind (content1 point-line-number point-column-number killed)
      (parse-state state-designator)
    (multiple-value-bind (content
                          mark-line-number mark-column-number mark-active-p)
        (parse-mark-state content1)
      (let* ((effective-line   (if point-line-supplied?
                                   point-line
                                   point-line-number))
             (effective-column (cond (point-column-supplied?
                                      point-column)
                                     ((and mark-line-number
                                           (= point-line-number
                                              mark-line-number)
                                           (< mark-column-number
                                              point-column-number))
                                      (1- point-column-number))
                                     (t
                                      point-column-number)))
             (buffer           (make-instance buffer-class))
             (site             (e:site buffer)))
        ;; Content, point cursor and mark cursor.
        (setf (buffer-string buffer) content)
        (when (or effective-line effective-column)
          (e:move-cursor-to-line
           (e:point site) effective-line effective-column))
        (when mark-line-number
          (let ((mark (e:set-mark site)))
            (e:move-cursor-to-line mark mark-line-number mark-column-number)
            (setf (e:mark-active-p site) mark-active-p)))
        ;; Insertion stack
        (when killed
          (let ((stack (e:insertion-stack site)))
            (mapc (lambda (text)
                    (setf (e:backward (e:push-entry stack)) text))
                  killed)))
        ;; Do it.
        (multiple-value-prog1
            (funcall function buffer)
          (check-leaked-cursors buffer))))))

(defmacro with-buffer ((buffer-var initial-state
                        &rest args &key buffer-class point-line point-column)
                       &body body)
  (declare (ignore buffer-class point-line point-column))
  `(call-with-buffer (lambda (,buffer-var) ,@body) ,initial-state ,@args))

;;; Assertions

(defun format-case (stream case &optional colon? at?)
  (declare (ignore colon? at?))
  (destructuring-bind (initial-state operation &optional (arguments '())) case
    (let ((input (a:ensure-car initial-state)))
      (format stream "~:[Expected~2*~;~:*For operation ~A~@[ ~{~S~^ ~}~] and ~
                      input~:@_~
                      ~:@_~
                      ~<|~@;~{~A|~^~@:_~}~:>~@:_~
                      ~:@_~
                      , expected~]"
              operation arguments (when input (list (split-into-lines input)))))))

(defun split-into-lines (string)
  (loop :with line = '()
        :for character :across string
        :if (char= character #\Newline)
          :collect (coerce (nreverse line) 'string) :into lines
          :and :do (setf line '())
        :else
          :do (push character line)
        :finally (return (append lines
                                 (list (coerce (nreverse line) 'string))))))

(defun is-cursor-state (expected-line expected-position
                        cursor-line cursor-position
                        &optional label case-description)
  (is (and (eql expected-line     cursor-line)
           (eql expected-position cursor-position))
      "~@<~/text.editing.test::format-case/ ~:[cursor~;~:*~A~] to be in ~
       position ~D:~D but it is in position ~:D:~D~@:>"
      case-description
      label expected-line expected-position cursor-line cursor-position))

(defun site-state (site)
  (let* ((point   (e:point site))
         (mark    (e:mark site))
         (stack   (e:insertion-stack site))
         (entries (e::entries stack))
         (killed  (map 'list #'e:insertion entries)))
    (values (c:line-number point)
            (c:cursor-position point)
            (when mark (c:line-number mark))
            (when mark (c:cursor-position mark))
            (when mark (e:mark-active-p site))
            killed)))

(defun is-site-state (expected-point-line expected-point-position
                      expected-mark-line  expected-mark-position
                      expected-mark-active-p
                      expected-killed
                      site
                      &optional case-description)
  (multiple-value-bind
        (point-line point-position mark-line mark-position mark-active-p
         killed)
      (site-state site)
    ;; Point cursor
    (is-cursor-state expected-point-line expected-point-position
                     point-line          point-position
                     "point" case-description)
    ;; Mark cursor
    (cond ((null expected-mark-line)
           (is (null mark-line)
               "~@<~/text.editing.test::format-case/ the mark to not be set ~
                but it is set.~@:>"
               case-description))
          (t
           (is-cursor-state expected-mark-line expected-mark-position
                            mark-line          mark-position
                            "mark" case-description)
           (is (eq expected-mark-active-p mark-active-p)
               "~@<~/text.editing.test::format-case/ the mark to be ~
                ~:[inactive~;active~] but it is ~
                ~:[inactive~;active~].~@:>"
               case-description expected-mark-active-p mark-active-p)))
    ;; Insertion stack
    (unless (eq expected-killed '*)
      (let ((expected-count (length expected-killed))
            (actual-count   (length killed)))
        (is (= expected-count actual-count)
            "~@<~/text.editing.test::format-case/ the insertion stack to ~
             contain ~D entr~:@P but it contains ~D entr~:@P.~@:>"
            case-description expected-count actual-count))
      (flet ((is-entry= (expected entry)
               (let ((expected (expand-newlines expected)))
                 (is (string= expected entry)
                     "~@<~/text.editing.test::format-case/ insertion string ~
                      to be ~S but it is ~S.~@:>"
                     case-description expected entry))))
        (map nil #'is-entry= expected-killed killed)))))

(defun is-site-state* (expected-state site
                       &optional initial-state operation arguments)
  (multiple-value-call #'is-site-state
    (values-list (rest (multiple-value-list
                        (parse-state-with-mark expected-state))))
    site (when initial-state (list initial-state operation arguments))))

(defun is-buffer-state (expected-content
                        expected-point-line expected-point-position
                        expected-mark-line  expected-mark-position
                        expected-mark-active-p
                        expected-killed
                        buffer
                        &optional case-description)
  ;; Buffer content
  (let ((content (buffer-string buffer)))
    (is (string= expected-content content)
        "~@<~/text.editing.test::format-case/ buffer to contain~:@_~
         ~:@_~
         ~<|~@;~{~A|~^~@:_~}~:>~@:_~
         ~:@_~
         but it contains~:@_~
         ~:@_~
         ~<|~@;~{~A|~^~@:_~}~:>~@:_~
         ~:@_~
         .~@:>"
        case-description
        (list (split-into-lines expected-content))
        (list (split-into-lines content))))
  ;; Site state
  (let ((site (e:site buffer)))
    (is-site-state expected-point-line expected-point-position
                   expected-mark-line  expected-mark-position
                   expected-mark-active-p
                   expected-killed
                   site
                   case-description)))

(defun is-buffer-state* (expected-state buffer
                         &optional initial-state operation arguments)
  (multiple-value-call #'is-buffer-state
    (parse-state-with-mark expected-state)
    buffer (when initial-state (list initial-state operation arguments))))

;;; Operation tests

(defun operation-case (operation arguments expected initial-state
                       &key target)
  (with-buffer (buffer initial-state)
    (flet ((do-it ()
             (ecase target
               (:point  (apply operation (e:point buffer) arguments))
               (:buffer (apply #'e:perform buffer operation arguments)))))
      (if (and (symbolp expected)
               (find-class expected nil))
          ;; Like `5am:signals' but 1) the expected condition is not
          ;; static 2) custom reason message
          (block nil
            (handler-bind
                ((condition (lambda (condition)
                              (when (typep condition expected)
                                (return)))))
              (do-it))
            (5am:fail "~@<~/text.editing.test::format-case/ a condition ~
                       of type ~A to be signaled but no condition was ~
                       signaled.~@:>"
                (list (a:ensure-car initial-state) operation arguments)
                expected))
          (let ((value (do-it)))
            (destructuring-bind
                (&key ((:state expected-state) initial-state)
                      ((:value expected-value) nil expected-value-supplied-p))
                (if (typep expected '(cons keyword))
                    expected
                    (list :state expected))
              (when expected-value-supplied-p
                (is (equalp expected-value value)))
              (unless (null expected-state)
                (is-buffer-state*
                 expected-state buffer
                 (a:ensure-car initial-state) operation arguments))))))))

(defun map-argument-combinations (function arguments)
  (if (null arguments)
      (list (funcall function))
      (apply #'a:map-product function (mapcar #'a:ensure-list arguments))))

(defun extract-expected (all-expected)
  (destructuring-bind (first-expected &rest rest-expected) all-expected
    (typecase first-expected
      ((cons (eql :always) (cons t null))
       (values (second first-expected) all-expected))
      ((cons (eql 2) (cons t null))
       (let ((expected (second first-expected)))
         (values expected (list* expected rest-expected))))
      ((cons integer (cons t null))
       (destructuring-bind (count expected) first-expected
         (values expected (list* (list (1- count) expected)
                                 rest-expected))))
      (t
       (values first-expected rest-expected)))))

(defmacro operation-cases ((operation &key (target :point)) &rest clauses)
  "Perform OPERATION (not evaluated) for test cases specified by CLAUSES.

TARGET (evaluated) controls on which level OPERATION is performed and
must be either `:point' or `:buffer'.

Each clause in CLAUSES is of the form

  ((ARGUMENT-DESIGNATOR₁ … ARGUMENT-DESIGNATORₙ)
   (INITIAL-STATE-DESIGNATOR₁ EXPECTED-DESIGNATOR₁₁ … EXPECTED-DESIGNATOR₁ₘ)
   (INITIAL-STATE-DESIGNATOR₂ EXPECTED-DESIGNATOR₂₁ … EXPECTED-DESIGNATOR₂ₘ)
   …)

where ARGUMENT-DESIGNATORₖ evaluates to either an atom or a list of
objects. OPERATION is (repeatedly) performed on a fresh buffer that is
initialized according to INITIAL-STATE with each of the combinations
of arguments that result from forming the Cartesian product of the
argument designators (so a single combination if all designators are
atoms). INITIAL-STATE-DESIGNATOR is of one of the forms

  \"INITIAL-BUFFER-STATE\"
  (\"INITIAL-BUFFER-STATE\" :killed INITIAL-INSERTION-STACK-STATE)

The k-th resulting buffer state and/or return value is checked against
EXPECTED-DESIGNATORₖ (evaluated) which evaluates to an object matching
one of the following forms:

  CONDITION-TYPE
  \"EXPECTED-BUFFER-STATE\"
  (\"EXPECTED-BUFFER-STATE\" :killed EXPECTED-INSERTION-STACK-STATE)
  (:value EXPECTED-VALUE [:state \"EXPECTED-BUFFER-STATE\"])

. When the last form is used and `:state' is not supplied, the
resulting buffer state must match INITIAL-BUFFER-STATE, that is the
operation must not modify the buffer state. Example:

  (operation-cases (e:delete :target :buffer)
    ((e:region (:forward :backward))
     (\"↑bar↧baz\" \"foo↑↧baz\" 'c:beginning-of-buffer)))

."
  (flet ((expand-clause (clause)
           (destructuring-bind ((&rest arguments) &rest sub-clauses) clause
             (flet ((expand-sub-clause (sub-clause)
                      (destructuring-bind (input &rest all-expected) sub-clause
                        (map-argument-combinations
                         (lambda (&rest arguments)
                           (let (expected)
                             (setf (values expected all-expected)
                                   (extract-expected all-expected))
                             `(operation-case ',operation (list ,@arguments)
                                              ,expected ,input
                                              :target ,target)))
                         arguments))))
               (a:mappend #'expand-sub-clause sub-clauses)))))
    `(progn ,@(a:mappend #'expand-clause clauses))))

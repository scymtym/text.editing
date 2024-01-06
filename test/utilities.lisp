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
    string))

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
        "~@<Expected no additional cursors to remain but ~ found ~A~@:>"
        leaked)))

(defun call-with-buffer (function state-designator
                         &key (buffer-class 'test-buffer)
                              (point-line   0            point-line-supplied?)
                              (point-column 0            point-column-supplied?))
  (multiple-value-bind (content1 point-line-number point-column-number killed)
      (parse-state state-designator)
    (multiple-value-bind (content mark-line-number mark-column-number)
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
  (destructuring-bind (operation arguments input ) case
    (format stream "~:[Expected~2*~;~:*For operation ~A~@[ ~{~S~^ ~}~] and ~
                    input~:@_~
                    ~:@_~
                    ~<|~@;~{~A|~^~@:_~}~:>~@:_~
                    ~:@_~
                    , expected~]"
            operation arguments (when input (list (split-into-lines input))))))

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

(defun buffer-state (buffer)
  (let* ((site    (e:site buffer))
         (point   (e:point site))
         (mark    (e:mark site))
         (stack   (e:insertion-stack site))
         (entries (e::entries stack))
         (killed  (map 'list #'e:insertion entries)))
    (values (buffer-string buffer)
            (c:line-number point)
            (c:cursor-position point)
            (when mark (c:line-number mark))
            (when mark (c:cursor-position mark))
            (when mark (e:mark-active-p site))
            killed)))

(defun is-buffer-state (expected-content
                        expected-point-line expected-point-position
                        expected-mark-line  expected-mark-position
                        expected-mark-active-p
                        expected-killed
                        buffer
                        &optional case-description)
  (multiple-value-bind
        (content
         point-line point-position mark-line mark-position mark-active-p
         killed)
      (buffer-state buffer)
    ;; Buffer content
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
        (list (split-into-lines content)))
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

(defun is-buffer-state* (expected-state buffer
                         &optional input operation arguments)
  (multiple-value-bind (content1 point-line-number point-column-number killed)
      (parse-state expected-state)
    (unless (and point-line-number point-column-number)
      (error "~@<State string ~S does not specify point.~@:>"
             expected-state))
    (multiple-value-bind (content
                          mark-line-number mark-column-number mark-active-p)
        (parse-mark-state content1)
      (when (and mark-line-number
                 (= point-line-number mark-line-number)
                 (< mark-column-number point-column-number))
        (decf point-column-number))
      (is-buffer-state content
                       point-line-number point-column-number
                       mark-line-number mark-column-number
                       mark-active-p
                       killed
                       buffer
                       (when input
                         (list operation arguments input))))))

;;; Operation tests

(defun operation-case (operation arguments expected initial-state
                       &key target)
  (with-buffer (buffer initial-state)
    (flet ((do-it ()
             (ecase target
               (:point  (apply operation (e:point buffer) arguments))
               (:buffer (apply #'e:perform buffer operation arguments)))))
      (case expected
        (cluffer:beginning-of-buffer
         (signals cluffer:beginning-of-buffer (do-it)))
        (cluffer:end-of-buffer
         (signals cluffer:end-of-buffer (do-it)))
        (e:insertion-stack-empty-error
         (signals e:insertion-stack-empty-error (do-it)))
        (t
         (do-it)
         (is-buffer-state*
          expected buffer
          (a:ensure-car initial-state) operation arguments))))))

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

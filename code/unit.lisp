;;;; unit.lisp --- Text units used in the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Utilities

(defun opposite-direction (direction)
  (ecase direction
    (:forward  :backward)
    (:backward :forward)))

;;; Unit definitions

;;; TODO same code in (at least) eclector, text.document-schema
(defclass unit () ()) ; abstract
(defmethod print-object ((object unit) stream)
  (let ((name (class-name (class-of object))))
    (multiple-value-bind (variable-name defined?) (macroexpand name)
      (cond ((and defined? (boundp variable-name))
             (format stream "~S" name))
            ((not *print-readably*)
             (print-unreadable-object (object stream :type t :identity t)))
            (t
             (error 'print-not-readable :object object))))))

(#-sbcl defvar #+sbcl sb-ext:defglobal **units** '())

(defun all-units ()
  **units**)

(defmacro define-unit (name (&rest super-unit-names) (&rest slot-specifiers)
                       &rest options)
  (let* ((super-units   (or super-unit-names '(unit)))
         (variable-name (alexandria:symbolicate '#:* name '#:*)))
    `(progn
       (defclass ,name ,super-units ,slot-specifiers
         ,@options)

       (#-sbcl defvar #+sbcl sb-ext:defglobal ,variable-name nil)
       (when (null ,variable-name)
         (setf ,variable-name (make-instance ',name)))
       (assert (typep ,variable-name 'unit))

       (define-symbol-macro ,name ,variable-name)

       (pushnew ,variable-name **units**)
       ',name)))

;;; Region

(defclass region-unit (unit) ()) ; abstract

(define-unit region (region-unit) ()
  (:documentation
   "The sequence of characters between point and mark.

It does not matter whether point or mark is closer to the beginning of
the buffer."))

(defclass region-or (region-unit)
  ((%fallback :initarg :fallback
              :reader  fallback))
  (:default-initargs
   :fallback (a:required-argument :fallback)))

;;; Buffer structure

(define-unit item () ()
  (:documentation
   "A single item, usually a character, in a buffer."))

(define-unit region-or-item (region-or)
  ()
  (:default-initargs
   :fallback *item*)
  (:documentation
   "The sequence of characters between point and mark or a single item.

Like the `region' unit if the mark is set and active, otherwise like
the `item' unit."))

(defclass line-unit (unit) ()) ; abstract

(define-unit line (line-unit) ()
  (:documentation
   "A line within a buffer."))

(define-unit semi-line (line-unit) ()
  (:documentation
   "The sequence from the cursor to the beginning or end of the line."))

(define-unit line-boundary (line-unit) ()
  (:documentation
   "An empty sequence at the beginning or end of the line."))

(defclass buffer-unit (unit) ()) ; abstract

(define-unit buffer (buffer-unit) ()
  (:documentation
   "The whole buffer."))

(define-unit semi-buffer (buffer-unit) ()
  (:documentation
   "The sequence from the cursor to one end of the buffer."))

(define-unit buffer-boundary (buffer-unit) ()
  (:documentation
   "An empty sequence at the beginning or end of the buffer."))

;;; Prose units

(defclass prose-unit (unit) ()) ; abstract

(define-unit word (prose-unit) ()
  (:documentation
   "A sequence of items that does not contain whitespace or punctuation
characters.

The beginning and end of the buffer also delimit words."))

(define-unit sentence (prose-unit) ()
  (:documentation
   "A sequence of word and whitespace items that is delimited by punctuation.

The beginning and end of the buffer also delimit sentences."))

(define-unit paragraph (prose-unit) ()
  (:documentation
   "A sequence of items delimited by two newlines.

The beginning and end of the buffer also delimit paragraphs."))

(define-unit page (prose-unit) ())

;;; Applies to all units

(defmethod apply-from-cursor :before ((continuation t)
                                      (cursor       c:cursor)
                                      (unit         t)
                                      (direction    (eql :forward)))
  (when (and (not (typep unit '(or line-unit buffer-unit c:cursor)))
             (c:end-of-buffer-p cursor))
    (error 'c:end-of-buffer)))

(defmethod apply-from-cursor :before ((continuation t)
                                      (cursor       c:cursor)
                                      (unit         t)
                                      (direction    (eql :backward)))
  (when (and (not (typep unit '(or line-unit buffer-unit c:cursor)))
             (c:beginning-of-buffer-p cursor))
    (error 'c:beginning-of-buffer)))

;;; Region

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         c:cursor)
                              (direction    (eql :forward)))
  (let ((from-cursor cursor)
        (to-cursor   unit))
    (apply-at-cursor-until continuation from-cursor
                           (lambda (cursor)
                             (or (c:cursor= cursor to-cursor)
                                 (c:end-of-buffer-p cursor)))
                           nil #'item-after-cursor*)))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         c:cursor)
                              (direction    (eql :backward)))
  (let ((from-cursor cursor)
        (to-cursor   unit))
    (apply-at-cursor-until continuation from-cursor
                           (lambda (cursor)
                             (or (c:cursor= cursor to-cursor)
                                 (c:beginning-of-buffer-p cursor)))
                           nil #'item-before-cursor*)))

;;; Item

(defmethod apply-from-cursor (continuation
                              (cursor    c:cursor)
                              (unit      item)
                              (direction (eql :forward)))
  (let ((item (item-after-cursor* cursor)))
    (funcall continuation cursor item)))

(defmethod apply-from-cursor (continuation
                              (cursor    c:cursor)
                              (unit      item)
                              (direction (eql :backward)))
  (let ((item (item-before-cursor* cursor)))
    (funcall continuation cursor item)))

;;; Line

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         semi-line)
                              (direction    (eql :forward)))
  (apply-at-cursor-until
   continuation cursor (a:disjoin #'c:end-of-buffer-p #'c:end-of-line-p)
   (constantly nil) #'item-after-cursor*))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         semi-line)
                              (direction    (eql :backward)))
  (apply-at-cursor-until
   continuation cursor (a:disjoin #'c:beginning-of-buffer-p #'c:beginning-of-line-p)
   (constantly nil) #'item-before-cursor*))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         line-boundary)
                              (direction    (eql :forward)))
  (c:end-of-line cursor))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         line-boundary)
                              (direction    (eql :backward)))
  (c:beginning-of-line cursor))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         line)
                              (direction    t))
  ;; What this does in most cases is moving the to the start/end of
  ;; the line then process all items contained in the line towards and
  ;; up to the opposite end of the line.
  (let ((opposite (opposite-direction direction)))
    (apply-from-cursor continuation cursor line-boundary opposite)
    (apply-from-cursor continuation cursor semi-line direction)))

;;; Prose units

;;; Word

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         word)
                              (direction    (eql :forward)))
  ;; Skip to word without applying effects.
  (apply-at-cursor-until
   (lambda (cursor item)
     (declare (ignore item))
     (move-item-forward cursor))
   cursor #'c:end-of-buffer-p #'word-constituent-p #'item-after-cursor*)
  ;; Operate on word.
  (apply-at-cursor-until
   continuation cursor
   #'c:end-of-buffer-p
   (complement #'word-constituent-p) #'item-after-cursor*))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         word)
                              (direction    (eql :backward)))
  ;; Skip to word without applying effects.
  (apply-at-cursor-until
   (lambda (cursor item)
     (declare (ignore item))
     (move-item-backward cursor))
   cursor #'c:beginning-of-buffer-p #'word-constituent-p #'item-before-cursor*)
  ;; Operate on word.
  (apply-at-cursor-until
   continuation cursor
   #'c:beginning-of-buffer-p
   (complement #'word-constituent-p) #'item-before-cursor*))

;;; Sentence

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         sentence)
                              (direction    (eql :forward)))
  (loop :while (end-of-sentence-p cursor)
        :do (move-item-forward cursor))
  (apply-at-cursor-until continuation cursor #'end-of-sentence-p
                         (constantly nil) #'item-after-cursor*))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         sentence)
                              (direction    (eql :backward)))
  (loop :while (beginning-of-sentence-p cursor)
        :do (move-item-backward cursor))
  (apply-at-cursor-until continuation cursor #'beginning-of-sentence-p
                         (constantly nil) #'item-before-cursor*))

;;; Paragraph

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         paragraph)
                              (direction    (eql :forward)))
  (let ((force-one? nil))
    (if (beginning-of-paragraph-p cursor)
        ;; If point is between paragraphs, that is at the beginning of
        ;; one paragraph and at the end of some other, process at
        ;; least one item.
        (setf force-one? (end-of-paragraph-p cursor))
        ;; If point is at the end of a paragraph but not at the
        ;; beginning of some other paragraph, move forward until no
        ;; longer at the end of a paragraph.
        (loop :while (end-of-paragraph-p cursor)
              :do (move-item-forward cursor)))
    (apply-at-cursor-until continuation cursor (lambda (cursor)
                                                 (if force-one?
                                                     (setf force-one? nil)
                                                     (end-of-paragraph-p cursor)))
                           (constantly nil) #'item-after-cursor*)))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         paragraph)
                              (direction    (eql :backward)))
  (loop :while (beginning-of-paragraph-p cursor)
        :do (move-item-backward cursor))
  (apply-at-cursor-until continuation cursor #'beginning-of-paragraph-p
                         (constantly nil) #'item-before-cursor*))

;;; Buffer

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         semi-buffer)
                              (direction    (eql :forward)))
  (apply-at-cursor-until continuation cursor #'c:end-of-buffer-p
                         (constantly nil) #'item-after-cursor*))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         semi-buffer)
                              (direction    (eql :backward)))
  (apply-at-cursor-until continuation cursor #'c:beginning-of-buffer-p
                         (constantly nil) #'item-before-cursor*))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         buffer-boundary)
                              (direction    (eql :forward)))
  (unless (c:end-of-buffer-p cursor) ; TODO already checked?
    (move-cursor-to-line cursor :last :end)))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         buffer-boundary)
                              (direction    (eql :backward)))
  (unless (c:beginning-of-buffer-p cursor) ; TODO already checked?
    (move-cursor-to-line cursor :first)))

(defmethod apply-from-cursor ((continuation t)
                              (cursor       c:cursor)
                              (unit         buffer)
                              (direction    t))
  ;; What this does in most cases is moving the to the start/end of
  ;; the buffer then process all items contained in the buffer towards
  ;; and up to the opposite end of the buffer.
  (let ((opposite (opposite-direction direction)))
    (apply-from-cursor continuation cursor buffer-boundary opposite)
    (apply-from-cursor continuation cursor semi-buffer direction)))

;;; Item transformers

(defmethod item-transformer ((transform t) (direction (eql :forward)))
  (let ((transform (a:ensure-function transform)))
    (lambda (cursor item)
      (cond ((char= item #\Newline) ; TODO make a predicate
             ;; No buffer change but transformer may change state.
             (funcall transform item)
             (move-item-forward cursor))
            (t
             (c:delete-item cursor)
             (c:insert-item cursor (funcall transform item)))))))

(defmethod item-transformer ((transform t) (direction (eql :backward)))
  (let ((transform (a:ensure-function transform)))
    (lambda (cursor item)
      (cond ((char= item #\Newline)
             ;; No buffer change but transformer may change state.
             (funcall transform item))
            (t
             (c:erase-item cursor)
             (c:insert-item cursor (funcall transform item))))
      (move-item-backward cursor))))

;;;; unit.lisp --- Text units provided by the expression nodule.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.expression)

;;; Unit definitions

(edit::define-unit expression ()
  ()
  (:documentation
   "A sequence of buffer items that correspond to a node in a syntax tree
associated with the buffer."))

(defmethod syntax-tree ((unit expression))
  :ast)

(edit::define-unit toplevel-expression (expression)
  ()
  (:documentation
   "The sequence of buffer items that correspond to the toplevel expression
node in a syntax tree associated with the buffer."))

(edit::define-unit region-or-expression (edit:region-or)
  ()
  (:default-initargs
   :fallback *expression*)
  (:documentation
   "The sequence of characters between point and mark or the innermost
expression containing point.

Like the `region' unit if the mark is set and active, otherwise like
the `expression' unit."))

;;; Methods

(flet ((forward (continuation cursor expression)
         (when (null expression)
           (error 'no-expression-after-cursor-error :cursor cursor))
         ;; Apply CONTINUATION until CURSOR reaches the end of
         ;; EXPRESSION.
         (with-cloned-cursor-at-expression-end
             (end-cursor cursor expression)
           (edit::apply-at-cursor-until
            continuation cursor
            (lambda (cursor)
              (or (cluffer:cursor= cursor end-cursor)
                  (cluffer:end-of-buffer-p cursor)))
            nil #'edit::item-after-cursor*)))
       (backward (continuation cursor expression)
         (when (null expression)
           (error 'no-expression-before-cursor-error :cursor cursor))
         ;; Apply CONTINUATION until CURSOR reaches the start of
         ;; EXPRESSION.
         (with-cloned-cursor-at-expression-start
             (start-cursor cursor expression)
           (edit::apply-at-cursor-until
            continuation cursor
            (lambda (cursor)
              (or (cluffer:cursor= cursor start-cursor)
                  (cluffer:beginning-of-buffer-p cursor)))
            nil #'edit::item-before-cursor*))))

  (defmethod edit:apply-from-cursor ((continuation t)
                                     (cursor       cluffer:cursor)
                                     (unit         expression)
                                     (direction    (eql :forward)))
    ;; Find innermost expression which contains CURSOR and move to the
    ;; end of that expression.
    (let* ((syntax-tree (syntax-tree unit))
           (expression  (maybe-advance-to-expression
                         cursor direction syntax-tree
                         :start-relation '<=
                         :end-relation   '<)))
      (forward continuation cursor expression)))

  (defmethod edit:apply-from-cursor ((continuation t)
                                     (cursor       cluffer:cursor)
                                     (unit         toplevel-expression)
                                     (direction    (eql :forward)))
    ;; Find outermost (that is toplevel) expression which either
    ;; contains CURSOR or follows CURSORS and move to the end of that
    ;; expression.
    (let* ((syntax-tree (syntax-tree unit))
           (initial     (outermost-expression-containing-cursor
                         cursor syntax-tree :start-relation '<=
                                            :end-relation   '<))
           (expression  (maybe-advance-to-expression
                         cursor direction syntax-tree
                         :start-relation     '<=
                         :end-relation       '<
                         :initial-expression initial
                         :advance-to-child   nil
                         :move-cursor        nil)))
      (unless (null expression)
        (move-to-expression-start cursor expression))
      (forward continuation cursor expression)))

  (defmethod edit:apply-from-cursor ((continuation t)
                                     (cursor       cluffer:cursor)
                                     (unit         expression)
                                     (direction    (eql :backward)))
    ;; Find innermost expression which contains CURSOR and move to the
    ;; beginning of that expression.
    (let* ((syntax-tree (syntax-tree unit))
           (expression  (maybe-advance-to-expression
                         cursor direction syntax-tree
                         :start-relation     '<
                         :end-relation       '<=)))
      (backward continuation cursor expression)))

  (defmethod edit:apply-from-cursor ((continuation t)
                                     (cursor       cluffer:cursor)
                                     (unit         toplevel-expression)
                                     (direction    (eql :backward)))
    ;; Find outermost (that is toplevel) expression which either
    ;; contains CURSOR or precedes CURSOR and move to the beginning of
    ;; that expression.
    (let* ((syntax-tree (syntax-tree unit))
           (initial     (outermost-expression-containing-cursor
                         cursor syntax-tree :start-relation '<
                                            :end-relation   '<=))
           (expression  (maybe-advance-to-expression
                         cursor direction syntax-tree
                         :start-relation     '<
                         :end-relation       '<=
                         :initial-expression initial
                         :advance-to-child   nil)))
      (unless (null expression)
        (move-to-expression-end cursor expression))
      (backward continuation cursor expression))))

;;;; motion.lisp --- Operations for cursor motion.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Movement primitives

(flet ((move-maybe-keeping-position (cursor new-line preferred-position)
         (let* ((preferred-position (when preferred-position
                                      (coerce-to-position
                                       preferred-position new-line)))
                (effective-position (min (or preferred-position
                                             (c:cursor-position cursor))
                                         (c:item-count new-line))))
           (move-cursor-to-line cursor new-line effective-position))))

  (defun move-line-forward (cursor &optional preferred-position)
    (check-type preferred-position (or null position-designator))
    (let ((next-line (next-line cursor)))
      (when (null next-line)
        (error 'c:end-of-buffer))
      (move-maybe-keeping-position cursor next-line preferred-position)))

  (defun move-line-backward (cursor &optional preferred-position)
    (check-type preferred-position (or null position-designator))
    (let ((previous-line (previous-line cursor)))
      (when (null previous-line)
        (error 'c:beginning-of-buffer))
      (move-maybe-keeping-position cursor previous-line preferred-position))))

;;; `move' methods

(macrolet ((define (direction mover)
             `(defmethod move ((cursor    c:cursor)
                               (unit      t)
                               (direction (eql ,direction)))
                (flet ((move-one (cursor item)
                         (declare (ignore item))
                         (,mover cursor)))
                  (declare (dynamic-extent #'move-one))
                  (apply-from-cursor #'move-one cursor unit direction)))))
  (define :forward  move-item-forward)
  (define :backward move-item-backward))

(macrolet ((define (direction mover)
             `(defmethod move ((cursor    c:cursor)
                               (unit      line)
                               (direction (eql ,direction)))
                (,mover cursor))))
  (define :forward  move-line-forward)
  (define :backward move-line-backward))

;;; Specialized operations

(defun back-to-indentation (cursor)
  (c:beginning-of-line cursor)
  (apply-at-cursor-until
   (lambda (cursor item)
     (declare (ignore item))
     (c:forward-item cursor))
   cursor #'c:end-of-line-p #'not-whitespace-p #'c:item-after-cursor))

;;; Site

(defmethod perform :around ((target    preferred-column-tracking-mixin)
                            (operation (eql 'move))
                            &rest operation-arguments)
  (destructuring-bind (unit direction &rest rest) operation-arguments
    (declare (ignore rest))
    (let ((point (point target)))
      (typecase unit
        (line
         ;; The preferred column is never null because a method on
         ;; (setf point) initializes it.
         (let ((preferred-column (preferred-column target)))
           (ecase direction
             (:forward  (move-line-forward point preferred-column))
             (:backward (move-line-backward point preferred-column)))))
        (t
         (call-next-method))))))

;;; Multiple cursors

(defmethod perform :before ((buffer    multiple-site-mixin)
                            (operation (eql 'move))
                            &rest operation-arguments)
  (let ((unit (first operation-arguments)))
    (when (and (typep unit 'buffer-boundary)
               (> (site-count buffer) 1))
      (cerror "Perform the operation anyway"
              'operation-would-collapse-sites-error
              :operation operation
              :arguments operation-arguments))))

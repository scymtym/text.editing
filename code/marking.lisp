;;;; marking.lisp --- Operations that are concerned with the mark cursor.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(defmethod mark-or-error ((object site))
  (or (mark object)
      (error 'mark-not-set-error :site object)))

(defmethod activate-mark ((site site))
  (mark-or-error site)
  (setf (mark-active-p site) t))
(operates-on-site activate-mark)

(defmethod deactivate-mark ((site site))
  (setf (mark-active-p site) nil))
(operates-on-site deactivate-mark)

(defmethod set-mark ((site site))
  (let* ((point    (point site))
         (old-mark (mark site))
         (new-mark (clone-cursor point :class 'cluffer-standard-line:left-sticky-cursor)))
    ;; TODO clone should attach
    ;; TODO there should be a multiple-value, setf-able cursor-position
    (move-cursor-to-line new-mark (c:line point) (c:cursor-position point))
    (when old-mark (push old-mark (mark-stack site)))
    (setf (mark-active-p site) t
          (mark          site) new-mark))) ; return new mark
(operates-on-site set-mark)

(defmethod set-mark-or-toggle-active ((site site))
  (let ((previous-operation (first (most-recent-operation site))))
    (cond ((eq previous-operation 'set-mark-or-toggle-active)
           (values nil (setf (mark-active-p site) (not (mark-active-p site)))))
          (t
           (set-mark site)
           (values t t)))))
(operates-on-site set-mark-or-toggle-active)

(defmethod pop-mark ((site site))
  (let* ((old-mark (mark-or-error site))
         (line     (c:line old-mark))
         (position (c:cursor-position old-mark))
         (new-mark (if (null (mark-stack site))
                       (error 'mark-stack-empty-error :site site)
                       (pop (mark-stack site))))
         (point    (point site)))
    (when old-mark
      (c:detach-cursor old-mark))
    (setf (mark          site) new-mark
          (mark-active-p site) nil)
    (move-cursor-to-line point line position)
    new-mark))
(operates-on-site pop-mark)

(defmethod exchange-point-and-mark ((site site))
  ;; TODO enable (rotatef (cursor-position (point site))
  ;;                      (cursor-position (mark-site)))
  (let* ((mark           (mark-or-error site))
         (mark-line      (c:line mark))
         (mark-position  (c:cursor-position mark))
         (point          (point site))
         (point-line     (c:line point))
         (point-position (c:cursor-position point)))
    (move-cursor-to-line point mark-line mark-position)
    (move-cursor-to-line mark point-line point-position)))
(operates-on-site exchange-point-and-mark)

(defmethod mark-object ((site      site)
                        (unit      t)
                        (direction t))
  (let ((point (point site))
        (mark  (mark site)))
    (cond ((null mark)
           (setf mark                 (clone-cursor point)
                 (mark          site) mark
                 (mark-active-p site) t)
           (move-cursor-to-line mark (c:line point) (c:cursor-position point)))
          ((not (mark-active-p site))
           (move-cursor-to-line mark (c:line point) (c:cursor-position point))
           (setf (mark-active-p site) t)))
    (move mark unit direction)))
(operates-on-site mark-object)

;;; Applying operations to the `region' unit
;;;
;;; The following functions are defined here because the `region' unit
;;; depends on the mark cursor.

(defgeneric compute-region-arguments (target operation unit)
  (:method ((target t) (operation t) (unit t))
    nil)
  (:method ((target c:cursor) (operation t) (unit c:cursor))
    (let ((point target)
          (mark  unit))
      (if (c:cursor< mark point)
          (values mark point)
          (values point mark))))
  (:method ((target site) (operation t) (unit region))
    (let ((point (point target))
          (mark  (mark-or-error target)))
      (unless (mark-active-p target)
        (error 'mark-not-active-error :site target :mark mark))
      (compute-region-arguments point operation mark)))
  (:method ((target site) (operation t) (unit region-or))
    (let ((mark (mark target)))
      (if (and mark (mark-active-p target))
          (compute-region-arguments (point target) operation mark)
          (compute-region-arguments target operation (fallback unit))))))

(defmethod perform ((target site) (operation t) &rest operation-arguments)
  (flet ((default ()
           (apply operation (point target) operation-arguments)))
    (destructuring-bind (&optional unit &rest more-arguments) operation-arguments
      (if (null unit)
          (default)
          (multiple-value-bind (new-target new-unit)
              (compute-region-arguments target operation unit)
            (if new-target
                (apply operation new-target new-unit more-arguments)
                (default)))))))
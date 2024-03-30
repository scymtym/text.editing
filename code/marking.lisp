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
  (let ((mark  (mark-or-error site))
        (point (point site)))
    (rotatef (cursor-location point) (cursor-location mark))))
(operates-on-site exchange-point-and-mark)

(defmethod mark-object ((site      site)
                        (unit      t)
                        (direction t))
  (let ((point (point site))
        (mark  (mark site)))
    (cond ((null mark)
           (setf mark                 (clone-cursor point)
                 (mark          site) mark
                 (mark-active-p site) t))
          ((not (mark-active-p site))
           (move-cursor-to-line mark (c:line point) (c:cursor-position point))
           (setf (mark-active-p site) t)))
    (let ((mover  (ecase direction
                    (:forward  #'move-item-forward)
                    (:backward #'move-item-backward)))
          (firstp t))
      (apply-from-cursor
       (lambda (cursor item)
         (declare (ignore item))
         ;; For some values of UNIT such as `expression' or
         ;; `toplevel-expression', in addition to MARK, point should
         ;; also move since the desired region extends in the
         ;; "opposite" direction starting from POINT.  If the sequence
         ;; swept over by CURSOR extends in the opposite direction
         ;; like that, move POINT to the first such location.
         (when firstp
           (when (ecase direction
                   (:forward  (c:cursor< cursor point))
                   (:backward (c:cursor< point cursor)))
             (move-cursor-to-line
              point (c:line cursor) (c:cursor-position cursor)))
           (setf firstp nil))
         (funcall mover cursor))
       mark unit direction))))
(operates-on-site mark-object)

;;; Applying operations to the `region' unit
;;;
;;; The following functions are defined here because the `region' unit
;;; depends on the mark cursor.

(defgeneric compute-region-arguments (target operation unit)
  (:method ((target c:cursor) (operation t) (unit c:cursor))
    (let ((point target)
          (mark  unit))
      (if (c:cursor< mark point)
          (values mark point)
          (values point mark))))
  (:method ((target site) (operation t) (unit t))
    (values (point target) unit))
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
  (let ((setfp (typep operation '(cons (eql setf)))))
    (destructuring-bind (&optional unit &rest more-arguments)
        (if setfp
            (rest operation-arguments)
            operation-arguments)
      (if (null unit)
          (let ((point (point target)))
            (if setfp
                (let ((new-value (first operation-arguments)))
                  (apply (fdefinition operation)
                         new-value point more-arguments))
                (apply operation point operation-arguments)))
          (multiple-value-bind (new-target new-unit)
              (compute-region-arguments target operation unit)
            (if setfp
                (let ((new-value (first operation-arguments)))
                  (apply (fdefinition operation)
                         new-value new-target new-unit more-arguments))
                (apply operation new-target new-unit more-arguments)))))))

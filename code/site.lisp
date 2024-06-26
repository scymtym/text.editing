;;;; site.lisp --- SITE class and associated methods.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Mixin class `preferred-column-tracking-mixin'

(defclass preferred-column-tracking-mixin ()
  ((%preferred-column :initarg  :preferred-column
                      :type     (or null unsigned-byte)
                      :accessor preferred-column
                      :initform nil)))

(defmethod (setf point) :after ((new-value c:cursor)
                                (object    preferred-column-tracking-mixin))
  (setf (preferred-column object) (c:cursor-position new-value)))

(defmethod perform :after ((target    preferred-column-tracking-mixin)
                           (operation t)
                           &rest operation-arguments)
  (unless (and (eq operation 'move)
               (destructuring-bind (unit &rest rest) operation-arguments
                 (declare (ignore rest))
                 (typep unit 'line)))
    (setf (preferred-column target) (c:cursor-position (point target)))))

;;; Mixin class `operation-history-mixin'

(defclass operation-history-mixin ()
  ((%operation-history :reader   operation-history
                       :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod most-recent-operation ((site operation-history-mixin))
  (let ((history (operation-history site)))
    (unless (a:emptyp history)
      (a:last-elt history))))

(defmethod push-operation ((operation t) (site operation-history-mixin))
  ;; TODO should this be unbounded?
  (vector-push-extend operation (operation-history site)))

(defmethod perform :around ((target operation-history-mixin) (operation t)
                            &rest operation-arguments)
  (multiple-value-prog1
      (call-next-method)
   (push-operation (list* operation operation-arguments) target)))

;;; Mixin class `site-data-mixin'

(defclass site-data-mixin ()
  ((%data :reader   %data
          :initform (make-hash-table))))

(defmethod data ((key t) (site site-data-mixin))
  (gethash key (%data site)))

(defmethod (setf data) ((new-value null) (key t) (site site-data-mixin))
  (remhash key (%data site))
  new-value)

(defmethod (setf data) ((new-value t) (key t) (site site-data-mixin))
  (setf (gethash key (%data site)) new-value))

;;; Class `site'

(defclass site (pi:print-items-mixin
                operation-history-mixin
                preferred-column-tracking-mixin
                site-data-mixin)
  ((%point           :initarg  :point
                     :accessor point)
   (%mark            :initarg  :mark
                     :accessor mark
                     :initform nil)
   (%mark-active-p   :initarg  :mark-active-p
                     :accessor mark-active-p
                     :initform nil)
   (%mark-stack      :accessor mark-stack
                     :type     list
                     :initform '())
   ;;
   (%insertion-stack :initarg  :insertion-stack
                     :reader   insertion-stack
                     :initform (make-instance 'insertion-stack))))

(defmethod shared-initialize :after ((instance   site)
                                     (slot-names t)
                                     &key buffer point)
  (cond  ((and buffer point)
          (error "The ~A and ~A initargs are mutually exclusive"
                 buffer point))
         (buffer
          (let ((initial-line (c:find-line buffer 0))
                (point        (make-instance 'cluffer-standard-line:right-sticky-cursor)))
            (c:attach-cursor point initial-line 0)
            (setf (point instance) point)))
         (point
          (assert (c:cursor-attached-p point))
          (setf (point instance) point))))

(defmethod pi:print-items append ((object site))
  (let ((point (point object)))
    (when (c:cursor-attached-p point)
      `((:point-line-number
         "~D" ,(cluffer-standard-buffer::maybe-safe-line-number point))
        ((:point-column-number (:after :point-line-number))
         ":~D" ,(c:cursor-position point))))))

(defmethod detach ((object site))
  (c:detach-cursor (point object))
  (a:when-let ((mark (mark object)))
    (c:detach-cursor mark)))

(defmethod perform :around ((target site) (operation t) &rest args)
  (declare (ignore args))
  ;; For signaled conditions of type `maybe-site-condition', set the
  ;; site slot if it is not already set.
  (handler-bind ((maybe-site-condition (lambda (condition)
                                         (when (null (site condition))
                                           (setf (%site condition) target)))))
    (call-next-method)))

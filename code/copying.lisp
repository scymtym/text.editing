;;;; copying.lisp --- Copy, kill and yank operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Yanking from insertion stack

(defmethod yank ((site site) (direction t) &key pop)
  (let* ((insertion-stack (insertion-stack site))
         (entry           (progn
                            (when pop
                              (pop-entry insertion-stack))
                            (or (top-entry insertion-stack)
                                (error 'insertion-stack-empty-error
                                       :site site))))
         (insertion       (insertion entry))
         (point           (point site)))
    (insert-items point insertion)
    (when (eq direction :backward)
      (loop :repeat (length insertion)
            :do (move-item-backward point)))))
(operates-on-site yank)

;;; Copying into insertion stack

(defmethod copy ((site site) (unit t) (direction t))
  (let ((entry (compute-insertion-stack-entry site 'copy unit direction)))
    (flet ((collect (target unit)
             (map-items (lambda (item)
                          (push item (forward entry)))
                        target unit direction)))
      (multiple-value-bind (new-target new-unit)
          (compute-region-arguments site 'copy unit)
        (cond (new-target
               (collect new-target new-unit)
               (setf (mark-active-p site) nil))
              (t
               (collect (point site) unit)))))
    entry))
(operates-on-site copy)

;;; `recording-cursor'
;;;
;;; A cursor that feeds deleted items into an insertion item.

(defclass recording-cursor (c:cursor)
  ((%target :initarg  :target
            :reader   target
            :documentation
            "The target cursor.")
   (%entry  :initarg  :entry
            :reader   entry
            :documentation
            "The insertion stack entry into which recorded items are stored.")))

(defmethod clone-cursor ((cursor recording-cursor) &rest args &key)
  (apply #'clone-cursor (target cursor) args))

(defmethod clone-initargs append ((cursor recording-cursor))
  (list :target (target cursor) :entry (entry cursor)))

;;; Forward cursor protocol methods to target cursor.
(macrolet ((define (method-name)
             `(defmethod ,method-name ((cursor recording-cursor))
                (,method-name (target cursor)))))
  (define c:buffer)
  (define c:cursor-attached-p)
  (define c:detach-cursor)
  (define c:line)
  (define c:cursor-position)
  (define c:join-line)
  (define delete-item-forward)
  (define delete-item-backward))

(defmethod c:attach-cursor ((cursor recording-cursor) (line t)
                            &optional (position nil positionp))
  (apply #'c:attach-cursor (target cursor) line (when positionp (list position))))

(defmethod (setf c:cursor-position) ((new-value t) (cursor recording-cursor))
  (setf (c:cursor-position (target cursor)) new-value))

(defmethod delete-item-forward :before ((cursor recording-cursor))
  (let ((target (target cursor)))
    (unless (c:end-of-buffer-p target)
      (push (item-after-cursor* target) (forward (entry cursor))))))

(defmethod delete-item-backward :before ((cursor recording-cursor))
  (let ((target (target cursor)))
    (unless (c:beginning-of-buffer-p target)
      (push (item-before-cursor* target) (backward (entry cursor))))))

;;; Bind `*site*' to the site for which an operation is currently
;;; being invoked.
(defvar *site* nil)
(defmethod perform ((target site) (operation (eql 'delete))
                    &rest operation-arguments)
  (declare (ignore operation-arguments))
  (let ((*site* target))
    (call-next-method)))

(defmethod delete :around ((cursor c:cursor) (unit t) (direction t))
  (let ((site *site*))
    (if (or (null site) (typep cursor 'recording-cursor))
        ;; No associated site or CURSOR already is a
        ;; `recording-cursor'.
        (call-next-method)
        ;; Either extend the top entry of the insertion stack or
        ;; create a new entry.
        (let* ((entry            (compute-insertion-stack-entry
                                  site 'delete unit direction))
               (recording-cursor (make-instance 'recording-cursor
                                                :target cursor
                                                :entry  entry)))
          (delete recording-cursor unit direction)))))

;;; These could be become exported protocols later.
(defgeneric compute-insertion-stack-entry (site operation unit direction
                                           &rest operation-arguments)
  (:method ((site t) (operation t) (unit t) (direction t)
            &rest operation-arguments)
    ;; Generally, create a new insertion stack entry if the previous
    ;; operation was not the same as the current operation. Otherwise
    ;; extend the top insertion stack item.
    (let* ((insertion-stack (insertion-stack site))
           (previous-entry  (top-entry insertion-stack)))
      (if (new-insertion-stack-entry-p
           site operation unit direction previous-entry operation-arguments)
          (push-entry insertion-stack)
          previous-entry))))

(defgeneric new-insertion-stack-entry-p (site operation unit direction
                                         previous-entry
                                         &rest operation-arguments)
  (:method ((site t) (operation t) (unit t) (direction t) (previous-entry t)
            &rest operation-arguments)
    (declare (ignore operation-arguments))
    (let ((previous-operation (most-recent-operation site)))
      (not (eq (first previous-operation) operation)))))

;;;; buffer-mixins.lisp --- Mixin classes for buffer classes.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Mixin class `site-mixin'

(defclass site-mixin ()
  ((%site :initarg  :site
          :accessor site)))

(defmethod initialize-instance :after ((instance site-mixin)
                                       &key (buffer instance))
  (setf (site instance) (make-instance 'site :buffer buffer)))

;;; Convenience methods

(defmethod point ((object site-mixin))
  (point (site object)))

(defmethod mark ((object site-mixin))
  (mark (site object)))

;;; Site protocol

(defmethod (setf site) :before ((new-value t) (buffer site-mixin))
  (assert (eq (c:buffer (point new-value)) buffer)))

(defmethod site-count ((buffer site-mixin))
  1)

(defmethod map-sites ((function t) (buffer site-mixin))
  (funcall function (site buffer)))

(defmethod sites ((buffer site-mixin))
  (list (site buffer)))

;;; Mixin class `multiple-site-mixin'

(defclass multiple-site-mixin ()
  ((%other-sites :type     list
                 :reader   other-sites
                 :accessor %other-sites
                 :initform '())))

(defmethod site-count ((buffer multiple-site-mixin))
  (+ (if (next-method-p) (call-next-method) 0)
     (length (other-sites buffer))))

(defmethod map-sites ((function t) (buffer multiple-site-mixin))
  (when (next-method-p) (call-next-method))
  (mapc function (other-sites buffer)))

(defmethod sites ((buffer multiple-site-mixin))
  (if (next-method-p)
      (append (call-next-method) (other-sites buffer))
      (other-sites buffer)))

(defmethod add-site ((site site) (buffer multiple-site-mixin))
  ;; TODO define specialized condition types
  (assert (eq (c:buffer (point site)) buffer))
  (a:when-let ((mark (mark site)))
    (assert (eq (c:buffer mark) buffer)))
  (assert (not (member site (sites buffer))))
  (push site (%other-sites buffer))
  site)

(defmethod remove-site ((site site) (buffer multiple-site-mixin))
  ;; TODO define specialized condition type
  (assert (member site (sites buffer)))
  (assert (not (eq site (site buffer))))
  (a:removef (%other-sites buffer) site)
  (detach site)
  site)

(flet ((make-site (site buffer line position)
         (let* ((point     (point site))
                (new-point (clone-cursor point)))
           (attach-cursor new-point line :buffer buffer :position position)
           (let* ((insertion-stack     (insertion-stack site))
                  (new-insertion-stack (clone insertion-stack))
                  (new-site            (make-instance
                                        'site :point           new-point
                                              :insertion-stack new-insertion-stack)))
             (values new-site new-point)))))

  (defmethod push-site-at ((buffer   multiple-site-mixin)
                           (line     t)
                           (position t))
    (let ((new-site (make-site (site buffer) buffer line position)))
      (add-site new-site buffer)))

  (defmethod push-site-relative ((buffer    multiple-site-mixin)
                                 (unit      t)
                                 (direction t))
    (let* ((site     (or (first (other-sites buffer)) (site buffer)))
           (point    (point site))
           (line     (c:line point))
           (position (c:cursor-position point)))
      (multiple-value-bind (new-site new-point)
          (make-site site buffer line position)
        (a:unwind-protect-case ()
            (progn
              (move new-point unit direction)
              (add-site new-site buffer))
          (:abort (detach new-site)))))))

(defmethod pop-site ((buffer multiple-site-mixin))
  (let ((other-sites (other-sites buffer)))
    (when (null other-sites)
      (error 'singular-site-error))
    (remove-site (first other-sites) buffer)))

(defmethod rotate-sites ((buffer    multiple-site-mixin)
                         (direction (eql :forward)))
  ;; TODO honor direction
  (let ((other-sites (other-sites buffer)))
    (when (zerop (length other-sites))
      (error 'singular-site-error))
    (psetf (site buffer)         (first other-sites)
           (%other-sites buffer) (append (rest other-sites)
                                         (list (site buffer))))))

(defmethod remove-other-sites ((buffer multiple-site-mixin))
  (mapc (a:rcurry #'remove-site buffer) (other-sites buffer)))

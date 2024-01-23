;;;; buffer-mixins.lisp --- Mixin classes for buffer classes.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.search)

;;; `site-search-state'
;;;
;;; Per site search state.

(declaim (inline site-state (setf site-state)))

(defun site-state (site)
  (e:data :incremental-search site))

(defun (setf site-state) (new-value site)
  (setf (e:data :incremental-search site) new-value))

(defclass site-search-state ()
  ((%start :initarg  :start
           :reader   start)
   (%match :initarg  :match
           :accessor match
           :initform nil))
  (:default-initargs
   :start (a:required-argument :start)))

(defmethod shared-initialize :after ((instance   site-search-state)
                                     (slot-names t)
                                     &key (start nil start-supplied?))
  (when start-supplied?
    (let ((new-start (e:clone-cursor start))) ; TODO make a `clone-cursor' that duplicates the line and column
      (e::attach-cursor new-start (c:line start) :position (c:cursor-position start))
      (setf (slot-value instance '%start) new-start))))

(defmethod e:detach ((object site-search-state))
  (c:detach-cursor (start object)))

(defmethod match ((object e:site))
  (a:when-let ((site-state (site-state object)))
    (match site-state)))

(defun update-site-for-search-state (site)
  (a:when-let ((match (match site)))
    (let ((end (end match)))
      (e:move-cursor-to-line
       (e:point site) (c:line end) (c:cursor-position end)))))

;;; `search-state-mixin'
;;;
;;; Mixin class for adding a search state to buffer classes.

(defclass search-state-mixin () ; TODO could be per-site or at least store some data per-site
  ((%search-state :accessor search-state
                  :initform nil)))

(defmethod add-match :around ((search-state t)
                              (buffer       search-state-mixin)
                              (start        c:cursor)
                              (end          c:cursor))
  (let* ((new-match (call-next-method))
         (start     (start new-match))
         ;; TODO do this once when the search state is initialized
         (sites     (cl:sort (e:sites buffer) #'c:cursor< :key #'e:point)))
    (loop :for site       :in sites
          :for site-state =   (site-state site)
          :for point      =   (start site-state)
          :for old-match  =   (match site-state)
          :while (c:cursor<= point start)
          :when (or (null old-match)
                    (c:cursor< start (start old-match)))
            :do (setf (match site-state) new-match))
    new-match))

(defmethod remove-match :before ((search-state t)
                                 (buffer       search-state-mixin)
                                 (match        match))
  (let* ((end   (end match))
         (next  (next match))
         (sites (cl:sort (e:sites buffer) #'c:cursor< :key #'e:point)))
    (loop :for site        :in sites
          :for point       =   (e:point site)
          :for site-state  =   (site-state site)
          :for site-match  =   (when site-state
                                 (match site-state))
          :while (c:cursor<= point end)
          :when (eq site-match match)
            :do (setf (match site-state) next))))

(defun add-site-search-states (buffer)
  (e:map-sites (lambda (site)
                 (let ((point (e:point site)))
                   (setf (site-state site)
                         (make-instance 'site-search-state :start point))))
               buffer))

(defun remove-site-search-states (buffer)
  (e:map-sites (lambda (site)
                 (a:when-let ((site-state (site-state site)))
                   (e:detach site-state)
                   (setf (site-state site) nil)))
               buffer))

(defun reset-site-search-states (buffer)
  (e:map-sites (lambda (site)
                 (a:when-let ((site-state (site-state site)))
                   (let* ((point    (e:point site))
                          (start    (start site-state))
                          (line     (c:line start))
                          (position (c:cursor-position start)))
                     (e::move-cursor-to-line point line position))))
               buffer))

(defun add-search-state (buffer start)
  (setf (search-state buffer) (make-instance 'search-state :start start))
  (add-site-search-states buffer))

(defun remove-search-state (buffer)
  (remove-site-search-states buffer)
  (setf (search-state buffer) nil))

(defun reset-search-state (buffer)
  (reset-site-search-states buffer))

(defun update-buffer-for-search-state (buffer)
  (e:map-sites #'update-site-for-search-state buffer))

(defmethod description ((buffer search-state-mixin) &key comment)
  (description (search-state buffer) :comment comment))

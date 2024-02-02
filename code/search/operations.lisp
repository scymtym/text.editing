;;;; operations.lisp --- Operations provided by the search module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.search)

(defun %target-buffer (target)
  (etypecase target
    (c:buffer
     target)
    (e:site
     (%target-buffer (e:point target)))
    (c:cursor
     (let ((buffer (c:buffer target)))
       (when (eq target (e:point buffer))
         buffer)))))

;;; With multiple sites, the following two methods are called for each
;;; site but we since the methods manipulate the global search state,
;;; they do all the work when they are called for the primary point
;;; and do nothing for other cursors.
(macrolet ((with-target-buffer ((buffer-var target) &body body)
             (a:once-only (target)
               `(a:when-let ((,buffer-var (%target-buffer ,target)))
                  ,@body)))
           (with-incremental-search-state ((state-var buffer-var target) &body body)
             (let ((state-var  (or state-var  (gensym "SEARCH-STATE")))
                   (buffer-var (or buffer-var (gensym "BUFFER"))))
               `(with-target-buffer (,buffer-var ,target)
                  (let ((,state-var (search-state ,buffer-var)))
                    (when (null ,state-var)
                      (error 'not-in-incremental-search-error
                             :buffer ,buffer-var))
                    ,@body)))))

  ;; Basic search

  (defmethod search ((target t) (query sequence) (direction t))
    ;; TODO honor direction
    (with-target-buffer (buffer target)
      (let ((search-state (make-instance 'search-state :start target
                                                       :query query)))
        (add-site-search-states buffer)
        (initial-matches search-state)
        (let ((result (not (a:emptyp (matches search-state)))))
          (when result
            (update-buffer-for-search-state buffer))
          (remove-site-search-states buffer)
          result))))

  ;; Incremental search

  (defmethod incremental-search ((target t) (direction t))
    (with-target-buffer (buffer target)
      (let ((search-state (search-state buffer)))
        (unless (null search-state)
          (error 'already-in-incremental-search-error :buffer buffer)))
      (add-search-state buffer target)
      (update-buffer-for-search-state buffer)))

  (defmethod finish-incremental-search ((target t))
    (with-incremental-search-state (search-state buffer target)
      (finish search-state)
      (remove-search-state buffer)))

  (defmethod abort-incremental-search ((target t))
    (with-incremental-search-state (search-state buffer target)
      (abort search-state)
      (reset-search-state buffer)
      (remove-search-state buffer)))

  (defmethod convert-matches-to-sites ((target t))
    (with-incremental-search-state (search-state buffer target)
      (let* ((primary-site  (e:site buffer))
             ;; The match may be `nil'.
             (primary-match (match (site-state primary-site))))
        ;; Add sites.
        (map nil (lambda (match)
                   (unless (eq match primary-match)
                     (let* ((end    (end match))
                            (line   (c:line end))
                            (column (c:cursor-position end)))
                       (e:push-site-at buffer line column))))
             (matches search-state))
        ;; Remove matches, keep point of primary site where it was.
        (finish search-state)
        (remove-search-state buffer))))

  (defmethod (setf case-mode) ((new-value t) (target t))
    (with-incremental-search-state (search-state buffer target)
      (setf (case-mode search-state) new-value)
      (update-buffer-for-search-state buffer)))

  (defmethod extend-query ((target t) (item t))
    (with-incremental-search-state (search-state buffer target)
      (extend-query search-state item)
      (update-buffer-for-search-state buffer)))

  (defmethod truncate-query ((target t) &key (count 1))
    (with-incremental-search-state (search-state buffer target)
      (truncate-query search-state :count count)
      (update-buffer-for-search-state buffer))))

(macrolet ((define (name accessor wrap condition)
             `(progn
                (defmethod ,name ((target e:site) &key (wrap-around t))
                  (let ((site-state (site-state target)))
                    (unless site-state
                      (let ((buffer (c:buffer (e:point target))))
                        (error 'not-in-incremental-search-error :buffer buffer)))
                    ;; MATCH can be null if the query does not occur
                    ;; in BUFFER or if the point of TARGET was
                    ;; positioned after (in case of searching forward)
                    ;; the last occurrence.
                    (let* ((match    (match site-state))
                           new-match)
                      (cond ((null match)
                             (let* ((buffer  (c:buffer (e:point target)))
                                    (matches (matches (search-state buffer))))
                               (if (or (a:emptyp matches) (not wrap-around))
                                   (error ',condition :site target)
                                   (setf (match site-state) ,wrap))))
                            ((setf new-match (,accessor match))
                             (setf (match site-state) new-match))
                            (wrap-around
                             (let* ((buffer  (c:buffer (e:point target)))
                                    (matches (matches (search-state buffer))))
                               (setf (match site-state) ,wrap)))
                            (t
                             (error ',condition :site target))))))

                (defmethod e:perform ((target e:site) (operation (eql ',name))
                                      &rest args &key wrap-around)
                  (declare (ignore wrap-around))
                  (apply (function ,name) target args))

                (defmethod e:perform :after ((target    c:buffer)
                                             (operation (eql ',name))
                                             &key wrap-around)
                  (declare (ignore wrap-around))
                  (update-buffer-for-search-state target)))))
  (define next-match
    next     (aref matches 0)                     no-next-match-error)
  (define previous-match
    previous (aref matches (1- (length matches))) no-previous-match-error))

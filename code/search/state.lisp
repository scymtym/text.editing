;;;; state.lisp --- State classes and manipulation functions for the search module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.search)

;;; `match'

;;; TODO make a span class somewhere?
(defclass match (pi:print-items-mixin)
  ((%state    :initarg  :state
              :reader   state
              :documentation
              "Overall search state.")
   ;; Next and previous matches
   (%next     :initarg  :next
              :accessor next
              :initform nil)
   (%previous :initarg  :previous
              :accessor previous
              :initform nil)
   ;; Start and end cursors
   (%start    :initarg  :start
              :type     c:cursor
              :reader   start)
   (%end      :initarg  :end
              :type     c:cursor
              :reader   end))
  (:default-initargs
   :state (a:required-argument :state)
   :start (a:required-argument :start)
   :end   (a:required-argument :end)))

(defmethod pi:print-items append ((object match))
  (let ((start (start object))
        (end   (end object)))
    `(,@(when (c:cursor-attached-p start)
          (let ((start-line   (cluffer-standard-buffer::maybe-safe-line-number start))
                (start-column (c:cursor-position start)))
            `((:start-line                          "~D"  ,start-line)
              ((:start-column (:after :start-line)) ":~D" ,start-column))))
      ,@(when (c:cursor-attached-p end)
          (let ((end-line   (cluffer-standard-buffer::maybe-safe-line-number end))
                (end-column (c:cursor-position end)))
            `(((:end-line   (:after :start-column)) " ~D" ,end-line)
              ((:end-column (:after :end-line))     ":~D" ,end-column)))))))

(defmethod e:detach ((object match))
  (unlink object)
  (c:detach-cursor (start object))
  (c:detach-cursor (end object)))

(defmethod link ((first match) (second match))
  (setf (next     first)  second
        (previous second) first))

(defmethod unlink ((match match))
  (let ((previous (previous match))
        (next     (next match)))
    (when previous
      (setf (next previous) next))
    (when next
      (setf (previous next) previous))))

(defmethod extend-query ((state match) (item t))
  (let ((end-cursor (end state)))
    (cond ((item-matches-p (state state) state item)
           (e:move-item-forward end-cursor)   ; TODO could be more than one
           t)
          (t
           nil))))

;;; `search-state'

;;; TODO toggles: regex search, whitespace matching
(defclass search-state (pi:print-items-mixin)
  (;; Query
   (%start     :type     c:cursor
               :reader   start
               :documentation
               "A cursor that indicates the buffer position at which the search
started. Typically, the point cursor is copied when the search starts
to produce this cursor.")
   (%query     :reader   query
               :initform (make-array 8 :adjustable t :fill-pointer 0)
               :documentation
               "The query sequence that should be matched against the buffer
content. The vector is mutated when the query is extended or
truncated.")
   (%case-mode :accessor case-mode
               :type     (member :ignore :match)
               :initform :ignore
               :documentation
               "Controls whether the query should be matched against the buffer
content in a case sensitive manner.")
   ;; Matches
   (%matches   :accessor matches
               :type     (or null (vector t))
               :initform nil))
  (:default-initargs
   :start (a:required-argument :start)))

(defmethod shared-initialize :after ((instance search-state) (slot-names t)
                                     &key (start nil start-supplied?)
                                          (query nil query-supplied?))
  (when start-supplied?
    (let ((new-start (e:clone-cursor start))) ; TODO make a `clone-cursor' that duplicates the line and column
      (e::attach-cursor new-start (c:line start) :position (c:cursor-position start))
      (setf (slot-value instance '%start) new-start)))
  (when query-supplied?
    (let ((stored-query (query instance))
          (new-length   (length query)))
      (when (< (length stored-query) new-length)
        (adjust-array stored-query (* 2 new-length)))
      (setf (fill-pointer stored-query) new-length)
      (replace stored-query query))))

(defmethod pi:print-items append ((object search-state))
  (let* ((query        (query object))
         (pretty-query (if (every (a:of-type 'character) query)
                           (coerce query 'string)
                           query))
         (match-count  (match-count object)))
    `((:query                         "~S"                        ,pretty-query)
      ((:match-count (:after :query)) " ~D match~:*~[es~;~:;es~]" ,match-count))))

(defmethod e:detach ((object search-state))
  (map nil #'e:detach (matches object))
  (c:detach-cursor (start object)))

(defmethod match-count ((search-state search-state))
  (length (matches search-state)))

(defmethod map-matches ((function t) (search-state search-state))
  (map nil function (matches search-state)))

(defmethod add-match ((search-state search-state)
                      (buffer       c:buffer)
                      (start        c:cursor)
                      (end          c:cursor))
  (let ((start-cursor (e:clone-cursor start)) ; TODO don't clone here?
        (end-cursor   end))
    (e::attach-cursor start-cursor (c:line start) :position (c:cursor-position start))
    (let ((match (make-instance 'match :state search-state
                                       :start start-cursor
                                       :end   end-cursor)))
      (vector-push-extend match (matches search-state))
      match)))

(defmethod remove-match ((search-state search-state)
                         (buffer       c:buffer)
                         (match        match))
  (e:detach match))

(defmethod initial-matches ((search-state search-state))
  (setf (matches search-state) (make-array 0 :adjustable t :fill-pointer 0))
  (let* ((query  (query search-state))
         (start  (start search-state)) ; TODO add end
         (buffer (c:buffer start)))
    ;; Move a temporary scan cursor through the buffer and collect
    ;; matches.
    (e:with-cloned-cursor (scan start)
      (e:move-cursor-to-line scan 0)    ; search from beginning
      (loop :with first-item = (aref query 0)
            :with previous = nil
            :until (c:end-of-buffer-p scan)
            :do (when (item-matches-p search-state scan first-item)
                  (let ((match (e:clone-cursor scan)))
                    (e::attach-cursor match (c:line scan) :position (c:cursor-position scan))
                    (if (loop :for item :across query
                              :always (item-matches-p search-state match item)
                              :do (e:move-item-forward match))
                        (let ((match (add-match search-state buffer scan match)))
                          (when previous
                            (link previous match))
                          (setf previous match))
                        (c:detach-cursor match))))
                (e:move-item-forward scan)))))

;;; Called after operations that do not permit incremental updates of
;;; the search state.
(defmethod rebuild-state ((search-state search-state))
  (let ((query   (query search-state))
        (matches (matches search-state))
        (buffer  (c:buffer (start search-state)))) ; TODO better way to get buffer
    (map nil (lambda (match)
               (remove-match search-state buffer match))
         matches)
    (setf (fill-pointer matches) 0)
    (unless (a:emptyp query)
      (initial-matches search-state))))

(defmethod (setf case-mode) :around ((new-value    t)
                                     (search-state search-state))
  (let ((old-value (case-mode search-state)))
    (prog1
        (call-next-method)
      (unless (eq new-value old-value)
        (rebuild-state search-state)))))

(defmethod extend-query ((state search-state) (item t))
  (vector-push-extend item (query state))
  (a:if-let ((matches (matches state)))
    (let ((buffer (c:buffer (start state)))) ; TODO better way to get the buffer?
      ;; TODO calling `remove-match' for each removed match
      ;; is inefficient because one of the methods scans
      ;; through all sites for each removed match
      (setf (matches state) (delete-if (lambda (match)
                                         (when (not (extend-query match item))
                                           (remove-match state buffer match)
                                           t))
                                       matches)))
    (initial-matches state)))

(defmethod extend-query ((state search-state) (item sequence))
  (map nil (a:curry #'extend-query state) item))

(defmethod truncate-query ((state search-state) &key (count 1))
  (when (plusp count)
    (let ((query (query state)))
      (when (< (length query) count)
        (error 'empty-query-error))
      (decf (fill-pointer query) count)
      (rebuild-state state))))

(defmethod finish ((search-state search-state))
  (e:detach search-state))

(defmethod abort ((search-state search-state))
  (e:detach search-state))

(defmethod description ((state search-state) &key comment)
  (let* ((query        (query state))
         (direction    :forward)
         (matches      (matches state)))
    (format nil "~@[~A; ~]Searching ~(~A~), ~(~A~)ing case: \"~A\"~
                 ~[~:; (~:D match~:*~[es~;~:;es~])~]"
            comment
            direction
            (case-mode state)
            (coerce query 'string)
            (length query) (length matches))))

;;; Incremental matching

(defmethod item-matches-p ((search-state search-state)
                           (match        match)
                           (query-item   t))
  (item-matches-p search-state (end match) query-item))

(defmethod item-matches-p ((search-state search-state)
                           (match        c:cursor)
                           (query-item   t))
  (and (not (c:end-of-buffer-p match))
       (let ((cursor-item (e::item-after-cursor* match)))
         (case (case-mode search-state)
           (:match  (eql cursor-item query-item))
           (:ignore (char-equal cursor-item query-item))))))

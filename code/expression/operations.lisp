;;;; operations.lisp --- Operations provided by the expression module.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing.expression)

;;; Structure editing

(defmethod raise ((cursor c:cursor) (unit expression) (direction t))
  (multiple-value-bind (expression ancestors)
      (multiple-value-bind (start-relation end-relation)
          (ecase direction
            (:forward  (values '<= '<))
            (:backward (values '<  '<=)))
        (maybe-advance-to-expression cursor direction (syntax-tree unit)
                                     :start-relation   start-relation
                                     :end-relation     end-relation
                                     :advance-to-child t
                                     :move-cursor      nil))
    (when (null expression)
      (error 'cursor-not-inside-expression-error :cursor cursor))
    (when (null ancestors)
      (error 'expression-at-toplevel-error :cursor     cursor
                                           :expression expression))
    (let ((parent (first ancestors)))
      ;; After identifying all sub-expressions, the general situation
      ;; should look like either this
      ;;
      ;;   PARENT
      ;;   ────────────────
      ;;   #(1 2 3 ↑ 4 5 6)
      ;;   xxxxxxxxxx─xxxxx
      ;;             EXPRESSION
      ;;
      ;; Sub-expression are indicated by over- and underlines, items
      ;; that should be deleted are indicated by x.
      (with-cloned-cursor-at-expression-start (parent-start cursor parent)
        (with-cloned-cursor-at-expression-end (parent-end cursor parent)
          (with-cloned-cursor-at-expression-start (start cursor expression)
            (with-cloned-cursor-at-expression-end (end cursor expression)
              (edit:delete parent-start start      :forward)
              (edit:delete end          parent-end :forward))))))))

(defmethod splice ((cursor c:cursor) (unit expression) (direction t))
  (let* ((syntax-tree (syntax-tree expression))
         (expression  (innermost-expression-containing-cursor
                       cursor syntax-tree :start-relation '<)))
    (when (null expression)
      (error 'cursor-not-inside-expression-error :cursor cursor))
    (let ((children (children expression)))
      (with-cloned-cursor-at-expression-start (start cursor expression)
        (edit:with-cloned-cursor (child-start start)
          (with-cloned-cursor-at-expression-end (end cursor expression)
            (edit:with-cloned-cursor (child-end end)
              (multiple-value-bind (first-child last-child emptyp)
                  (ecase direction
                    ((nil)
                     (values (first children) (a:lastcar children)))
                    ((:forward :backward)
                     ;; Find the first child after the cursor to keep
                     ;; all children either before or after that child
                     ;; depending on DIRECTION.
                     (edit:with-cloned-cursor (probe start)
                       (loop :for previous = nil :then child
                             :for (child . rest) :on children
                             :for done = (progn
                                           (move-to-expression-start
                                            probe child)
                                           (cluffer:cursor<= cursor probe))
                             :until done
                             :finally (return
                                        (ecase direction
                                          (:forward
                                           (values child
                                                   (a:lastcar rest)
                                                   (and children (not done))))
                                          (:backward
                                           (values (first children)
                                                   previous
                                                   (and children
                                                        (not previous))))))))))
                ;; If there are no children (either in general or on
                ;; the "right side" of CURSOR, just delete the
                ;; expression.
                (when emptyp
                  (edit:delete start end :forward)
                  (return-from splice))
                ;; Use either start/end of first/last child or move
                ;; inwards over delimiter in case there is no
                ;; respective child.
                (if (null first-child)
                    (move-delimiter-forward child-start)
                    (move-to-expression-start child-start first-child))
                (if (null last-child)
                    (move-delimiter-backward child-end)
                    (move-to-expression-end child-end last-child))
                ;; For DIRECTION being `:forward', after identifying
                ;; all sub-expressions, the general situation should
                ;; look something like this
                ;;
                ;;   EXPRESSION
                ;;   ────────────────
                ;;   #(1 2 3 ↑ 4 5 6)
                ;;   xxxxxxxxxx─   ─x
                ;;                 LAST-CHILD
                ;;             FIRST-CHILD
                ;;
                ;; Sub-expression are indicated by over- and
                ;; underlines, items that should be deleted are
                ;; indicated by x.
                (case direction
                  ((nil) ; delete only delimiters to preserve CURSOR position
                   (edit:delete start     child-start :forward)
                   (edit:delete child-end end         :forward))
                  (:forward ; CURSOR must go backward before spliced text :(
                   (let ((string (edit:items child-start child-end :forward)))
                     (setf (edit:items start end :forward) string)
                     (loop :repeat (length string)
                           :do (edit:move-item-backward cursor))))
                  (:backward ; CURSOR ends up after spliced text :)
                   (setf (edit:items start       end       :forward)
                         (edit:items child-start child-end :forward))))))))))))

(labels ((find-delimiters (cursor expression)
           (let* ((children    (children expression))
                  (first-child (first children))
                  (last-child  (a:lastcar children)))
             ;; After identifying all sub-expressions, the general
             ;; situation should look like either this
             ;;
             ;;   EXPRESSION
             ;;   ──────────────────────
             ;;   #(1 2 3  ↑  4 5 6)
             ;;   **─             ─*
             ;;     FIRST-CHILD   LAST-CHILD
             ;;
             ;; or this (when there are no children)
             ;;
             ;;   EXPRESSION
             ;;   ───────────
             ;;   "a b ↑ c d"
             ;;   *         *
             ;;
             ;; Sub-expression are indicated by over- and underlines,
             ;; items that should be copied as new delimiters are
             ;; indicated by *.
             (with-cloned-cursor-at-expression-start (start cursor expression)
               (edit:with-cloned-cursor (first-child-start start)
                 (if (null first-child)
                     (move-delimiter-forward first-child-start)
                     (move-to-expression-start first-child-start first-child))
                 (with-cloned-cursor-at-expression-end (end cursor expression)
                   (edit:with-cloned-cursor (last-child-end end)
                     (if (null last-child)
                         (move-delimiter-backward last-child-end)
                         (move-to-expression-end last-child-end last-child))
                     (let ((opening (edit:items start first-child-start :forward))
                           (closing (edit:items last-child-end end :forward)))
                       (values (trim opening t) (trim closing nil)))))))))
         (trim (items from-end)
           (a:if-let ((index (position-if-not #'edit:whitespacep items
                                              :from-end from-end)))
             (if from-end
                 (subseq items 0 (1+ index))
                 (subseq items index))
             items))
         (split-expression (cursor opening closing &optional separator)
           (edit:insert-items cursor closing)
           (unless (null separator)
             (edit:insert-items cursor separator))
           (edit:insert-items cursor opening)
           ;; CURSOR should be between the new expressions.
           ;; TODO avoid somehow. maybe
           ;; (insert-items ... :where :before) or allow changing the
           ;; stickyness of the cursor?
           (loop :repeat (length opening)
                 :do (edit:move-item-backward cursor))))

  (defmethod split ((cursor c:cursor) (unit expression))
    (let* ((syntax-tree (syntax-tree unit))
           (expression  (innermost-expression-containing-cursor
                         cursor syntax-tree :start-relation '<)))
      (when (null expression)
        (error 'cursor-not-inside-expression-error :cursor cursor))
      (multiple-value-call #'split-expression
        cursor (find-delimiters cursor expression))))

  (defmethod split ((cursor c:cursor) (unit toplevel-expression))
    (let* ((syntax-tree (syntax-tree unit))
           (expressions (expressions-containing-cursor
                         cursor syntax-tree :start-relation '<)))
      (when (null expressions)
        (error 'cursor-not-inside-expression-error :cursor cursor))
      (loop :for expression :in expressions
            :for (opening closing) = (multiple-value-list
                                      (find-delimiters cursor expression))
            :collect opening :into all-opening
            :collect closing :into all-closing
            :finally (split-expression
                      cursor
                      (apply #'concatenate 'vector (nreverse all-opening))
                      (apply #'concatenate 'vector all-closing)
                      #.(format nil "~2%"))))))

(defmethod join ((cursor c:cursor) (unit expression))
  (let* ((syntax-tree (syntax-tree unit))
         (expression1 (maybe-advance-to-expression
                       cursor :backward syntax-tree :move-cursor nil))
         (expression2 (maybe-advance-to-expression
                       cursor :forward syntax-tree :move-cursor nil)))
    (when (null expression1)
      (error 'no-expression-before-cursor-error :cursor cursor))
    (when (null expression2)
      (error 'no-expression-after-cursor-error :cursor cursor))
    (let ((last-child1  (a:lastcar (children expression1)))
          (first-child2 (first     (children expression2))))
      ;; After identifying all sub-expressions, the situation should
      ;; look like either this
      ;;
      ;;    EXPRESSION1   EXPRESSION2
      ;;    ───────────   ───────────
      ;;    #(11 22 33) ↑ #(44 55 66)
      ;;            ──x   xx──
      ;;   LAST-CHILD1      FIRST-CHILD2
      ;;
      ;; or this (the difference this case is the lack of LAST-CHILD1
      ;; and FIRST-CHILD2)
      ;;
      ;;   EXPRESSION1  EXPRESSION2
      ;;   ───────      ───────
      ;;   "a b c"  ↑   "d e f"
      ;;         x      x
      ;;
      ;; Sub-expression are indicated by over- and underlines, items
      ;; that should be deleted are indicated by x.
      (with-cloned-cursor-at-expression-end (end1 cursor expression1)
        (edit:with-cloned-cursor (child1-end end1)
          (if (null last-child1)
              (edit:move-item-backward child1-end)
              (move-to-expression-end child1-end last-child1))
          (with-cloned-cursor-at-expression-start (start2 cursor expression2)
            (edit:with-cloned-cursor (child2-start start2)
              (if (null first-child2)
                  (edit:move-item-forward child2-start)
                  (move-to-expression-start child2-start first-child2))
              (edit:delete child1-end end1         :forward)
              (edit:delete start2     child2-start :forward)
              ;; If joining the expressions would merge tokens,
              ;; insert whitespace to prevent that.
              ;; TODO this is not appropriate when merging strings
              (when (c:cursor= end1 start2)
                (edit:insert-item start2 #\Space)))))))))

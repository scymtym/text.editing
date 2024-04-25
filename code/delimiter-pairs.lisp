;;;; delimiter-pairs.lisp --- Operations on paired delimiters.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Delimiter pairs

(defvar *delimiter-pairs* '((#\( . #\))
                            (#\[ . #\])
                            (#\{ . #\})
                            (#\" . #\")
                            (#\| . #\|)))

(defun closing-delimiter (opening)
  (cdr (assoc opening *delimiter-pairs*)))

(defun closing-delimiter-or-lose (opening)
  (or (closing-delimiter opening)
      (error "~@<Cannot guess the closing delimiter for opening delimiter ~
              ~C.~@:>"
             opening)))

(defun opening-delimiter (closing)
  (car (rassoc closing *delimiter-pairs*)))

(opposite-pair opening-delimiter closing-delimiter)

;;; Operations

(defun insert-opening-and-closing-delimiter (cursor opening closing)
  (c:insert-item cursor opening)
  (c:insert-item cursor closing)
  (when (and (not (c:end-of-buffer-p cursor))
             (not (c:end-of-line-p cursor))
             (not (whitespacep (c:item-after-cursor cursor))))
    (c:insert-item cursor #\Space)
    (c:backward-item cursor))
  (c:backward-item cursor))

(defmethod insert-delimiter-pair
    ((cursor t) (opening t) &key (closing (closing-delimiter-or-lose opening)))
  (insert-opening-and-closing-delimiter cursor opening closing))

(defmethod maybe-move-past-closing-delimiter ((cursor t) (closing t)
                                              &key (whitespace :delete))
  (check-type whitespace (member nil :move-past :delete))
  (let (maybe-closing)
    ;; If WHITESPACE is `nil' look at the character immediately
    ;; following CURSOR.  If WHITESPACE is either `:move-past' or
    ;; `:delete' move past whitespace and look at the first character
    ;; after the whitespace.  If that character is equal to CLOSING
    ;; either move CURSOR or delete the whitespace items.
    (if (null whitespace)
        (setf maybe-closing (item-after-cursor* cursor))
        (with-cloned-cursor (scan cursor)
          (apply-at-cursor-until
           (lambda (cursor item)
             (declare (ignore item))
             (move-item-forward cursor))
           scan
           #'c:end-of-buffer-p (complement #'whitespacep) #'item-after-cursor*)
          (setf maybe-closing (item-after-cursor* scan))
          (when (eql maybe-closing closing)
            (ecase whitespace
              (:move-past (setf (cursor-location cursor)
                                (cursor-location scan)))
              (:delete (delete cursor scan :forward))))))
    (when (eql maybe-closing closing)
      (move-item-forward cursor)
      t)))

(defmethod move-past-closing-delimiter ((cursor t) (closing t)
                                        &key (whitespace :delete))
  (unless (maybe-move-past-closing-delimiter
           cursor closing :whitespace whitespace)
    (error 'no-closing-delimiter-error :cursor    cursor
                                       :delimiter closing)))

(defmethod move-past-closing-delimiter-or-insert-delimiter-pair
    ((cursor t) (delimiter t) &key (whitespace nil))
  (unless (maybe-move-past-closing-delimiter
           cursor delimiter :whitespace whitespace)
    (insert-opening-and-closing-delimiter cursor delimiter delimiter)
    t))

(macrolet ((define (direction)
             ;; I hope this not too "clever" but the two directions are so
             ;; symmetrical that i could not resist.
             (macrolet ((dir (name)
                          `(let ((name ',name))
                             (ecase direction
                               (:forward  name)
                               (:backward (find-opposite name))))))
               `(defmethod delete-delimiter-pair-or-item
                    ((cursor c:cursor) (direction (eql ,direction))
                     &key (if-not-empty nil))
                  ;; The initial `item-after-cursor*' may signal `end-of-buffer'
                  ;; which is good.
                  (let* ((maybe-delimiter (,(dir item-after-cursor*) cursor))
                         (direct          (,(dir opening-delimiter)
                                           maybe-delimiter))
                         opposite)
                    (cond ;; Item after (before) CURSOR is neither opening nor
                          ;; closing delimiter.
                          ((and (null (setf opposite (,(dir closing-delimiter)
                                                      maybe-delimiter)))
                                (null direct))
                           ;; Just delete single item after (before) CURSOR.
                           (,(dir delete-item-forward) cursor))
                          ;; (↑) immediately surrounded by delimiters
                          ((and (not (,(dir c:beginning-of-buffer-p) cursor))
                                (eql (,(dir item-before-cursor*) cursor) direct))
                           ;; Delete delimiters before and after CURSOR.
                           (,(dir delete-item-forward) cursor)
                           (,(dir delete-item-backward) cursor))
                          ;; Forward: ↑(); backward: ()↑
                          ((when (not (null opposite))
                             (with-cloned-cursor (probe cursor)
                               (,(dir move-item-forward) probe)
                               (and (not (,(dir c:end-of-buffer-p) probe))
                                    (eql (,(dir item-after-cursor*) probe)
                                         opposite))))
                           ;; Delete opening and closing delimiters after
                           ;; (before) CURSOR.
                           (loop :repeat 2
                                 :do (,(dir delete-item-forward) cursor)))
                          ;; The remaining cases are: forward: (stuff↑) or
                          ;; ↑(stuff); backward: (↑stuff) or ↑(stuff).
                          ((null if-not-empty)) ; do nothing
                          ((eq if-not-empty :move-past)
                           ;; Move CURSOR past delimiter.
                           (,(dir move-item-forward) cursor))
                          ((eq if-not-empty :delete-inside)
                           ;; For ↑(stuff) and (stuff↑), delete the stuff
                           ;; inside.
                           (cond ((null direct)
                                  (,(dir move-item-forward) cursor)
                                  (,(dir delete-item-forward) cursor)
                                  (,(dir move-item-backward) cursor))
                                 (t
                                  (,(dir delete-item-backward) cursor))))
                          ((typep if-not-empty '(or symbol function))
                           (funcall if-not-empty
                                    cursor
                                    (if opposite :outside :inside)))))))))
  (define :forward)
  (define :backward))

(defmethod surround-with-delimiter-pair
    ((cursor t) (unit t) (direction t) (opening t)
     &key (closing (closing-delimiter opening))
          (count   1))
  (with-cloned-cursor (opposite-cursor cursor)
    (loop :repeat count
          :do (move opposite-cursor unit direction))
    ;; TODO ensure that the items between CURSOR and OPPOSITE-CURSOR
    ;; contain either no delimiters or balanced delimiters. Maybe do
    ;; that in the expression module?
    (ecase direction
      (:forward
       (insert-item cursor opening)
       (insert-item opposite-cursor closing))
      (:backward
       (insert-item cursor closing)
       (insert-item opposite-cursor opening)
       (move-item-backward cursor)))))

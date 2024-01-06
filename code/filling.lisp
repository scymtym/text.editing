;;;; filling.lisp --- Line wrapping operations.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(defparameter *fill-column* 72)

(defmethod insert-words-fill ((cursor c:cursor) (words sequence)
                              &key prefix
                                   suffix
                                   per-line-prefix
                                   (fill-column    *fill-column*))
  (flet ((insert-word (word start-of-line-p)
           (etypecase word
             (character
              (c:insert-item cursor word))
             (sequence
              (unless (or start-of-line-p
                          (and (= (length word) 1)
                               (let ((first (aref word 0)))
                                 (or (whitespacep first) (punctuationp first)))))
                (c:insert-item cursor #\Space))
              (insert-items cursor word)))))
    (let ((start-of-line-p t)
          (firstp          t))
      (map nil (lambda (word)
                 (cond (firstp
                        (when prefix
                          (insert-items cursor prefix))
                        (setf firstp nil))
                       ((>= (+ (c:cursor-position cursor)
                               (etypecase word
                                 (character 1)
                                 (sequence  (length word))))
                            fill-column)
                        (c:split-line cursor)
                        (when per-line-prefix
                          (insert-items cursor per-line-prefix))
                        (setf start-of-line-p t)))
                 (insert-word word start-of-line-p)
                 (setf start-of-line-p nil))
           words))
    (when suffix
      (insert-items cursor suffix))))

(defmethod fill-words ((start-cursor c:cursor)
                       (end-cursor   c:cursor)
                       (words        sequence)
                       &rest args
                       &key prefix suffix per-line-prefix fill-column)
  (declare (ignore suffix per-line-prefix fill-column))
  (assert (c:cursor<= start-cursor end-cursor))
  (let ((start-line-number   (c:line start-cursor))
        (start-column-number (c:cursor-position start-cursor))
        (empty-region?       (c:cursor= start-cursor end-cursor)))
    ;; Separator to prevent cursors from merging.
    (when empty-region?
      (c:insert-item end-cursor #\|)
      (when (typep end-cursor 'cluffer-standard-line:left-sticky-cursor)
        (c:forward-item end-cursor))
      (when (typep start-cursor 'cluffer-standard-line:right-sticky-cursor)
        (c:backward-item start-cursor)))
    (apply #'insert-words-fill start-cursor words args)
    (delete-range start-cursor end-cursor)
    (let ((final-column (+ start-column-number (length prefix))))
      (move-cursor-to-line start-cursor start-line-number final-column))))

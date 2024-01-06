;;;; transposing.lisp --- Operations for transposing objects.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(defmethod transpose ((cursor c:cursor) (unit t) (direction t))
  (let* ((middle   cursor)
         (buffer   (c:buffer middle))
         (line     (c:line middle))
         (position (c:cursor-position middle))
         (first    '())
         (second   '()))
    (with-temporary-cursor (start buffer :line line :position position)
      (with-temporary-cursor (end buffer :line line :position position)
        (move start unit :backward)
        (move end unit :forward)
        (map-to-cursor! (lambda (cursor)
                          (push (item-after-cursor* cursor) second)
                          (delete-item-forward cursor))
                        middle end)
        (map-to-cursor! (lambda (cursor)
                          (push (item-after-cursor* cursor) first)
                          (delete-item-forward cursor))
                        start middle)
        (insert-items middle (nreverse second))
        (insert-items middle (nreverse first))
        (when (eq direction :backward)
          (loop :repeat (+ (length first) (length second))
                :do (move-item-backward cursor)))))))

;;; TODO transpose line

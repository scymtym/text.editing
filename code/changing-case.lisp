;;;; changing-case.lisp --- Operations for changing the case of characters.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

(defun make-capitalizer ()
  (let ((state :prefix))
    (lambda (item)
      ;; Upcase first character of non-whitespace spans, downcase the
      ;; remaining characters. Whitespace remains unchanged.
      (labels ((process ()
                 (case state
                   (:whitespace
                    (cond ((or (lower-case-p item) (upper-case-p item))
                           (setf state :prefix)
                           (process))
                          (t
                           item)))
                   (:prefix
                    (cond ((or (lower-case-p item) (upper-case-p item))
                           (setf state :suffix)
                           (char-upcase item))
                          (t
                           item)))
                   (:suffix
                    (cond ((or (lower-case-p item) (upper-case-p item))
                           (char-downcase item))
                          (t
                           (setf state :whitespace)
                           (process)))))))
        (process)))))

(defmethod change-case ((cursor t) (unit t) (direction t) (case t))
  (let* ((operation   (ecase case
                        (:down    #'char-downcase)
                        (:up      #'char-upcase)
                        (:capital (make-capitalizer))))
         (transformer (item-transformer operation direction)))
    (apply-from-cursor transformer cursor unit direction)))

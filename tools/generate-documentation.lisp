(cl:defpackage #:text.editing.generate-documentation
  (:use
   #:cl)

(cl:in-package #:text.editing.generate-documentation)

;;; Utilities

(defun emit-section (title &key (level "section"))
  (format t "@node ~A~%@~A ~2:*~A~2%" title level))

(defun emit-string (string)
  (let ((stream *standard-output*))
    (loop :for character :across string
          :do (case character
                (#\{ (write-string "@{" stream))
                (#\} (write-string "@}" stream))
                (t   (write-char character stream))))))

(cl:defpackage #:text.editing.generate-documentation
  (:use
   #:cl)

  (:local-nicknames
   (#:a    #:alexandria)))

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

;;; Emacs equivalence

(defun read-equivalence-data ()
  (a:with-input-from-file (stream "../data/emacs-equivalence.sexp")
    (let ((*package* (find-package "TEXT.EDITING")))
      (read stream))))

(defun emit-operation-equivalence (operation-data)
  (destructuring-bind (operation parameters &rest clauses) operation-data
    (let ((count (1+ (length parameters))))
      (format t "@multitable @columnfractions ~{~,2F~^ ~}~@
                 @headitem ~{~24@<~@(~A~)~> @tab ~}~A~%"
              (append (make-list count :initial-element (/ .6d0 count))
                      '(.4d0))
              (list* "Operation" parameters) "Equivalent Emacs Command")
      (labels ((node (node remaining-parameters acc)
                 (if (null remaining-parameters)
                     (destructuring-bind (command &key arguments key mismatch)
                         (a:ensure-list node)
                       (format t "@item     ~{~24@<@t{~(~S~)}~> @tab ~}~
                                  @t{~A~@[ ~{~A~^ ~}~]}~@[ (@kbd{~A})~]~
                                  ~@[ (@emph{~A})~]~%"
                               (list* operation (reverse acc))
                               command arguments
                               (when key
                                 (with-output-to-string (*standard-output*)
                                   (emit-string key)))
                               mismatch))
                     (destructuring-bind (argument &rest rest-node) node
                       (mapc (lambda (node)
                               (node node
                                     (rest remaining-parameters)
                                     (list* argument acc)))
                             rest-node)))))
        (mapc (a:rcurry #'node parameters '()) clauses)))
    (format t "@end multitable~%")))

(defun emit-equivalence-section (section-data)
  (format t "~2%")
  (destructuring-bind (title &rest operation-clauses) section-data
    (emit-section title)
    (mapc #'emit-operation-equivalence operation-clauses)))

(defun emit-emacs-equivalence ()
  (let ((data (read-equivalence-data)))
    (a:with-output-to-file
        (*standard-output* "generated-chapter-equivalent-emacs-commands.texi"
                           :if-exists :supersede)
      (format t "@node Equivalent Emacs Commands~@
                 @chapter Equivalent Emacs Commands~@
                 ~@
                 @menu~@
                 ~{* ~A::~%~}~
                 @end menu"
              (mapcar #'first data))
      (mapcar #'emit-equivalence-section data))))

(emit-emacs-equivalence)

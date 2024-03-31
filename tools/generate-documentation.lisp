(cl:defpackage #:text.editing.generate-documentation
  (:use
   #:cl)

  (:local-nicknames
   (#:a    #:alexandria)

   (#:edit #:text.editing)))

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
                (#\@ (write-string "@@" stream))
                (t   (write-char character stream))))))

;;; Unit descriptions and unit graph

(defun emit-unit-description (unit)
  (let* ((class            (class-of unit))
         (superclasses     (sb-mop:class-direct-superclasses class))
         (name             (class-name class))
         (superclass-names (mapcar #'class-name superclasses))
         (documentation    (or (documentation class t)
                               "@emph{not documented}")))
    (format t "@item~%")
    (format t "@tindex ~(~A~)~%" name)
    (format t "@cindex ~(~A~) unit~%" name)
    (format t "@anchor{unit-~(~A~)}~:*@t{~(~S~)} @tab ~{~(~A~)~^, ~}@tab ~A~2%"
            name superclass-names documentation)))

(defstruct node value children)

(defun emit-unit-graph (stream)
  (let* ((all       (edit:all-units))
         (remaining all)
         (by-class  (make-hash-table :test #'eq)))
    (labels ((add (unit-class)
               (or (gethash unit-class by-class)
                   (let ((node (make-node :value unit-class)))
                     (a:removef remaining unit-class :key #'class-of)
                     (setf (gethash unit-class by-class) node)
                     (mapc (lambda (subclass)
                             (let ((sub-node (add subclass)))
                               (push sub-node (node-children node))))
                           (sb-mop:class-direct-subclasses unit-class))
                     node))))
      (let* ((unit (find-class 'edit:unit))
             (root (add unit)))
        (unless (null remaining)
          (error "~@<Missed unit~P ~{~A~^, ~}.~@:>"
                 (length remaining) remaining))
        (format stream "@example~%")
        (let ((utilities.print-tree:*use-unicode?* nil))
          (utilities.print-tree:print-tree
           *standard-output* root
           (utilities.print-tree:make-node-printer
            (lambda (stream depth node)
              (declare (ignore depth))
              (let* ((class (node-value node))
                     (name  (class-name class)))
                (if (member class all :key #'class-of)
                    (format stream "@ref{unit-~(~A~),~:*~(~S~)}" name)
                    (write-string (string-downcase name) stream))))
            nil #'node-children)))
        (format stream "~&@end example~%")))))

(defun emit-unit-descriptions ()
  (a:with-output-to-file (*standard-output* "generated-builtin-units.texi"
                                            :if-exists :supersede)
    (let ((title "Built-in Units"))
      (emit-section title :level "subsection")
      ;; Unit class table
      (format t "~@<The following units are provided by the @sysname{} ~
                 library by default.  Users of the library can define ~
                 additional units.~@:>~2%")
      (format t "@multitable @columnfractions .2 .2 .6~%")
      (format t "@headitem Name @tab Super-units @tab Description~%")
      (let ((sorted (sort (copy-list (edit:all-units)) #'string<
                          :key (a:compose #'class-name #'class-of))))
        (mapc #'emit-unit-description sorted))
      (format t "~2%@end multitable~2%")
      ;; Unit class hierarchy
      (format t "~@<The hierarchy of built-in unit classes looks like ~
                 this:~@:>~2%")
      (emit-unit-graph *standard-output*))))

(emit-unit-descriptions)

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
                               (if (keywordp operation)
                                   (make-list (1+ (length acc))
                                              :initial-element "@emph{not implemented}")
                                   (list* operation (reverse acc)))
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

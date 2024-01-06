;;;; predicates.lisp --- Predicates used by the text.editing system.
;;;;
;;;; Copyright (C) 2023, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.editing)

;;; Item level

(declaim (inline whitespacep not-whitespace-p
                 sentence-punctuation-p other-punctuation-p punctuationp))
(defun whitespacep (character)
  (member character '(#\Space #\Tab #\Newline #\Page)))

(defun not-whitespace-p (character)
  (not (whitespacep character)))

(defun sentence-punctuation-p (character)
  (member character '(#\. #\? #\! #\: #\, #\;)))

;;; TODO repeated in incrementalist
(defun other-punctuation-p (character)
  (member character '(#\- #\/
                      #\( #\) #\[ #\] #\< #\> #\{ #\} #\")))

(defun punctuationp (character)
  (or (sentence-punctuation-p character)
      (other-punctuation-p character)))

(defun word-constituent-p (character)
  (not (or (whitespacep character) (punctuationp character))))

;;; Cursor level

(defun beginning-of-sentence-p (cursor)
  ;; Look for at least one whitespace item before CURSOR preceded by
  ;; beginning-of-buffer or sentence punctuation.
  (or (c:beginning-of-buffer-p cursor)
      (and ;; For performance: quick check for whitespace before
           ;; cloning CURSOR.
           (whitespacep (item-before-cursor* cursor))
           (with-cloned-cursor (cursor cursor)
             (move-item-backward cursor)
             (loop :while (and (not (c:beginning-of-buffer-p cursor))
                               (whitespacep (item-before-cursor* cursor)))
                   :do (move-item-backward cursor)
                   :finally (return
                              (or (c:beginning-of-buffer-p cursor)
                                  (sentence-punctuation-p
                                   (item-before-cursor* cursor)))))))))

(defun end-of-sentence-p (cursor)
  ;; Look for sentence punctuation followed by end-of-buffer or at
  ;; least one whitespace.
  (or (c:end-of-buffer-p cursor)
      (and (whitespacep (item-after-cursor* cursor))
           (not (c:beginning-of-buffer-p cursor))
           (with-cloned-cursor (cursor cursor)
             (move-item-backward cursor)
             (sentence-punctuation-p (item-after-cursor* cursor))))))

(defun beginning-of-paragraph-p (cursor)
  (or (c:beginning-of-buffer-p cursor)
      (and (c:beginning-of-line-p cursor)
           (with-cloned-cursor (cursor cursor)
             (move-item-backward cursor)
             (c:beginning-of-line-p cursor)))))

(defun end-of-paragraph-p (cursor)
  (or (c:end-of-buffer-p cursor)
      (and (c:beginning-of-line-p cursor)
           (not (c:beginning-of-buffer-p cursor))
           (with-cloned-cursor (cursor cursor)
             (move-item-backward cursor)
             (c:beginning-of-line-p cursor)))))

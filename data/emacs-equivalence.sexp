(("Motion"
  (move (unit direction)
   (item
    (:forward  ("forward-char"  :key "C-f"))
    (:backward ("backward-char" :key "C-b")))
   (line
    (:forward  ("next-line"     :key "C-n"))
    (:backward ("previous-line" :key "C-p")))
   (semi-line
    (:forward  "end-of-line")
    (:backward "beginning-of-line"))
   (line-boundary
    (:forward  ("end-of-line"       :key "C-a"))
    (:backward ("beginning-of-line" :key "C-e")))
   (buffer
    (:forward  "end-of-buffer")
    (:backward "beginning-of-buffer"))
   (semi-buffer
    (:forward  "end-of-buffer")
    (:backward "beginning-of-buffer"))
   (buffer-boundary
    (:forward  ("end-of-buffer"       :key "M->"))
    (:backward ("beginning-of-buffer" :key "M-<")))
   (word
    (:forward  ("forward-word"  :key "M-f"))
    (:backward ("backward-word" :key "M-b")))
   (sentence
    (:forward ("forward-sentence"   :key "M-e"))
    (:backward ("backward-sentence" :key "M-a")))
   (paragraph
    (:forward ("forward-paragraph"   :key "M-}"))
    (:backward ("backward-paragraph" :key "M-{")))
   (page
    (:forward  ("forward-page"  :key "C-x ]"))
    (:backward ("backward-page" :key "C-x [")))))
 ("Deletion"
  (delete (unit direction)
   (region
    ;; There is also `delete-region' which does not touch the kill
    ;; ring.
    (:forward  ("kill-region" :arguments ("beg" "end")))
    (:backward ("kill-region" :arguments ("beg" "end"))))
   (item
    (:forward  ("delete-char"          :arguments (1) :key "C-d"))
    (:backward ("delete-backward-char" :arguments (1) :key "<backspace>")))
   (line
    (:forward  ("kill-line" :key "C-k"))
    (:backward ("kill-line" :key "with 0 prefix (C-0 C-k)")))
   (semi-line
    (:forward  ("kill-line"                :key "C-k"))
    (:backward ("kill-line" :arguments (0) :key "with 0 prefix (C-0 C-k)")))
   (buffer
    (:forward  ("erase-buffer"))
    (:backward ("erase-buffer")))
   (word
    (:forward  ("kill-word"          :arguments (1) :key "M-d"))
    (:backward ("backward-kill-word" :arguments (1) :key "M-<backspace>")))
   (sentence
    (:forward  ("kill-sentence"          :key "M-k"))
    (:backward ("backward-kill-sentence" :key "C-x DEL")))
   (paragraph
    (:forward  ("kill-paragraph"          :arguments (1)))
    (:backward ("backward-kill-paragraph" :arguments (1))))
   (expression
    (:forward  ("kill-sexp"          :arguments (1) :key "C-M-k"))
    (:backward ("backward-kill-sexp" :arguments (1) :key "C-M-<backspace>"))))
  (delete-indentation ()
   "delete-indentation")
  (delete-trailing-whitespace ()
   "delete-trailing-whitespace"))
 ("Marking"
  (mark-object (unit direction)
   (buffer
    (:forward  ("mark-whole-buffer" :key "C-x h")))
   (semi-buffer
    (:forward  ("mark-end-of-buffer"))
    (:backward ("mark-beginning-of-buffer")))
   (word
    (:forward ("mark-word" :key "M-@")))
   (sentence
    (:forward  ("mark-end-of-sentence"))
    (:backward ("mark-beginning-of-sentence")))
   (paragraph
    (:forward ("mark-paragraph" :key "M-h")))
   (page
    (:forward ("mark-page" :key "C-x C-p")))))
 ("Transformation"
  (change-case (unit direction case)
   (:region
    (:forward
     (:up      ("upcase-region"     :arguments ("beg" "end") :key "C-x C-u"))
     (:down    ("downcase-region"   :arguments ("beg" "end") :key "C-x C-l"))
     (:capital ("capitalize-region" :arguments ("beg" "end")))))
   (item
    (:forward
     (:up      ("upcase-char"     :arguments (1)
                                  :mismatch  "Emacs does not move point"))
     (:down    ("downcase-char"   :arguments (1)
                                  :mismatch  "Emacs does not move point"))
     (:capital ("capitalize-char" :arguments (1)
                                  :mismatch  "Emacs does not move point"))))
   (word
    (:forward
     (:up      ("upcase-word"     :arguments (1) :key "M-u"))
     (:down    ("downcase-word"   :arguments (1) :key "M-l"))
     (:capital ("capitalize-word" :arguments (1) :key "M-c")))))
  (transpose (unit direction)
   (item
    (:forward ("transpose-chars" :arguments (1) :key "C-t")))
   (line
    (:forward ("transpose-lines" :key "C-x C-t")))
   (word
    (:forward ("transpose-words" :arguments (1) :key "M-t")))
   (sentence
    (:forward ("transpose-sentences" :arguments (1))))
   (paragraph
    (:forward ("transpose-paragraphs" :arguments (1))))
   (expression
    (:forward ("transpose-sexps" :key "C-M-t")))))
 ("Paredit"
  (:wrap (unit)
   (:expression ("paredit-wrap-sexp" :arguments ("argument" "open" "close"))))
  (:splice (unit)
   (:expression ("paredit-splice-sexp" :key "M-s")))
  (:raise (unit)
   (:expression ("paredit-raise-sexp" :key "M-r")))))

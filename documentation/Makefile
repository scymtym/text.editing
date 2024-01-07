SBCL ?= sbcl
QUICKLISP_DIRECTORY ?= $${HOME}/quicklisp/
ASDF_SOURCE_REGISTRY ?= (quote (:source-registry (:directory "'$$(pwd)/..'") :ignore-inherited-configuration))

NAME = text.editing

SOURCES = $(NAME).texi $(wildcard chapter-*.texi)

GENERATED = generated-version.texi                           \
            generated-chapter-equivalent-emacs-commands.texi \
            generated-builtin-units.texi
# generated-chapter-changelog.texi

FIGURES = $(wildcard *.png)

.PHONY: all clean

all : $(NAME).info $(NAME).pdf $(NAME).html

INPUTS = $(SOURCES) $(GENERATED)

generated-version.texi: ../version-string.sexp
	$(SBCL) --noinform --disable-debugger --no-sysinit --no-userinit                        \
	     --eval '(require :asdf)'                                                           \
	     --eval '(format t "@macro sysversion~%~A~%@end macro" (uiop:read-file-form "$<"))' \
	     --quit                                                                             \
	  > $@

generated-builtin-units.texi generated-chapter-equivalent-emacs-commands.texi: \
  ../tools/generate-documentation.lisp                                         \
  ../code/unit.lisp                                                            \
  ../data/emacs-equivalence.sexp
	$(SBCL) --noinform --disable-debugger --no-sysinit --no-userinit     \
	  --load "$(QUICKLISP_DIRECTORY)/setup.lisp"                         \
	  --eval '(asdf:initialize-source-registry $(ASDF_SOURCE_REGISTRY))' \
	  --eval '(ql:quickload "text.editing")'                             \
	  --eval '(ql:quickload "utilities.print-tree")'                     \
	  --load '../tools/generate-documentation.lisp'                      \
	  --quit

#generated-chapter-changelog.texi: ../changes.sexp ../tools/write-changelog.lisp
#	$(SBCL) --script write-changelog.lisp > $@

$(NAME).info: $(INPUTS) $(FIGURES)
	makeinfo --output $@ $(NAME).texi

$(NAME).pdf: $(INPUTS) $(FIGURES)
	texi2pdf $(NAME).texi

$(NAME).html: $(INPUTS) $(FIGURES) $(wildcard *.css)
	makeinfo --html --no-split --no-headers \
	         --css-include=style-common.css --css-include=style-single.css \
	         $(NAME).texi
        # Texinfo produce text.html for some reason
	@if [ -e text.html ] ; then \
	    mv text.html $@ ;       \
	fi

clean:
	rm -f *.cp *.cps *.fn *.fns \
	      *.aux *.toc *.log \
	      $(NAME).info $(NAME).pdf $(NAME).html \
	      $(GENERATED)

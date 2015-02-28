CASK  ?= cask
EMACS ?= emacs

EMACSFLAGS =
EMACSBATCH = $(EMACS) --batch -Q -L . $(EMACSFLAGS)

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = inf-purescript.el        \
	purescript.el           \
	purescript-mode.el      \
	purescript-font-lock.el

OBJS = $(SRCS:.el=.elc)

.PHONY: all compile clean

all: compile

compile: $(OBJS)

package: Cask
	$(CASK) package

clean:
	$(RM) $(OBJS)

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

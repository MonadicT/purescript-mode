SED   ?= sed
CASK  ?= cask
CURL  ?= curl

EMACS = emacs
EMACSFLAGS =

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS   := $(shell $(CASK) files)
OBJECTS = $(SRCS:.el=.elc)

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: all build clean

all: build

build: $(OBJECTS)

package: $(PKGDIR)
	$(CASK) package

clean:
	$(CASK) clean-elc

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<

purescript-font-lock.el:
	$(CURL) -L -o $@ "https://github.com/haskell/haskell-mode/raw/master/haskell-font-lock.el"
	$(SED) -i -e 's/Haskell/PureScript/g' -e 's/haskell/purescript/g' $@

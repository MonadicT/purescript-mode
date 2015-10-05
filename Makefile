SED   ?= sed
CASK  ?= cask
CURL  ?= curl

EMACS = emacs

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

.PHONY: all build clean

all: build

build: $(PKGDIR)
	$(CASK) build

package: $(PKGDIR)
	$(CASK) package

clean:
	$(CASK) clean-elc

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

.PHONY: purescript-font-lock.el
purescript-font-lock.el:
	$(CURL) -L -o $@ "https://github.com/haskell/haskell-mode/raw/master/haskell-font-lock.el"
	$(SED) -i -e 's/Haskell/PureScript/g' -e 's/haskell/purescript/g' $@

CASK  ?= cask
EMACS ?= emacs

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

.PHONY: all build clean

all: build

build: Cask
	$(CASK) build

package: Cask
	$(CASK) package

clean:
	$(CASK) clean-elc

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

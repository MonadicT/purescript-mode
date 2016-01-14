SED   ?= sed
CASK  ?= cask
CURL  ?= curl

EMACS = emacs
EMACSFLAGS =

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS   := \
	purescript-mode.el \
	purescript-font-lock.el
OBJECTS = $(SRCS:.el=.elc)

AUTOLOADS = purescript-mode-autoloads.el

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: all build clean

all: build $(AUTOLOADS)

build: $(OBJECTS)

package: $(PKGDIR)
	$(CASK) package

clean:
	$(CASK) clean-elc
	$(RM) $(AUTOLOADS) $(AUTOLOADS:.el=.elc)

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<

$(AUTOLOADS): $(ELFILES) purescript-mode.elc
	$(EMACSBATCH) \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq generated-autoload-file "$(CURDIR)/$@")' \
		-f batch-update-autoloads "."

purescript-font-lock.el:
	$(CURL) -L -o $@ "https://github.com/haskell/haskell-mode/raw/master/haskell-font-lock.el"
	$(SED) -i -e 's/Haskell/PureScript/g' -e 's/haskell/purescript/g' $@

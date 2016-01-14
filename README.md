# purescript-mode

[![Travis build status](https://travis-ci.org/emacs-pe/purescript-mode.svg?branch=master)](https://travis-ci.org/emacs-pe/purescript-mode)

> **WARNING**: package in alpha stage.

Emacs major mode for [PureScript][]

## Installation

### Using [use-package][]

You need to add `emacs-pe` archive to your package-archives:

``` elisp
(add-to-list 'package-archives
             '("emacs-pe" . "https://emacs-pe.github.io/packages/"))
```

Install packages:

``` elisp
(use-package purescript-mode            ; PureScript mode
  :ensure t
  :pin emacs-pe)

(use-package psci                       ; psci integration
  :ensure t
  :pin emacs-pe)

(use-package flycheck-purescript        ; Setup Flycheck by PureScript projects
  :ensure t
  :pin emacs-pe
  :init (add-hook 'purescript-mode-hook #'flycheck-purescript-setup))
```

### From git repository

1. Clone repository

``` bash
git clone https://github.com/emacs-pe/purescript-mode.git
```

2. Change directory to the cloned repository and create package autoloads:

``` bash
make
```

2. Add the following to your `init.el`:

``` el
(add-to-list 'load-path "/path/to/cloned/purescript-mode/")
(require 'purescript-mode-autoloads)
```

## Usage:

### Indentation:
You can use [haskell-mode indentation](https://github.com/haskell/haskell-mode/wiki/Indentation), e.g:

```elisp
(add-hook 'purescript-mode-hook #'haskell-indentation-mode)
```

## Acknowledgements
This mode ~~steals~~ borrows many ideas from [haskell-mode][].

## Related Projects
+ @dysinger's [purescript-mode](https://github.com/dysinger/purescript-mode) repurposes [haskell-mode][] for PureScript.

[PureScript]: http://www.purescript.org/
[haskell-mode]: https://github.com/haskell/haskell-mode "Emacs mode for Haskell"
[use-package]: https://github.com/jwiegley/use-package "A use-package declaration for simplifying your .emacs"

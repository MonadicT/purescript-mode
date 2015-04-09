# purescript-mode

[![Travis build status](https://travis-ci.org/emacs-pe/purescript-mode.svg?branch=master)](https://travis-ci.org/emacs-pe/purescript-mode)

> **WARNING**: package in alpha stage.

Emacs major mode for [PureScript][]

## Usage:

### Indentation:
You can use [haskell-mode indentation](https://github.com/haskell/haskell-mode/wiki/Indentation), e.g:

```elisp
(add-hook 'purescript-mode-hook #'haskell-indentation-mode)
```

## TODO
+ [ ] Add support for flycheck.

## Acknowledgements
This mode ~~steals~~ borrows many ideas from [haskell-mode][].

## Related Projects
+ @dysinger's [purescript-mode](https://github.com/dysinger/purescript-mode) repurposes [haskell-mode][] for PureScript.

[PureScript]: http://www.purescript.org/
[haskell-mode]: https://github.com/haskell/haskell-mode "Emacs mode for Haskell"

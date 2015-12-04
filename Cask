(source gnu)
(source melpa)

(package "purescript-mode" "0.1" "PureScript major mode.")

(files "psci.el"
       "purescript-mode.el"
       "purescript-font-lock.el"

       "psc-ide.el"
       "flycheck-purescript.el"
       )

(depends-on "queue")                    ; psc-ide
(depends-on "let-alist")                ; psc-ide
(depends-on "flycheck")                 ; flycheck-purescript

(development
 (depends-on "ert")
 (depends-on "undercover"))

;;; purescript-font-lock.el --- Font locking module for PureScript Mode -*- lexical-binding: t -*-

;; Copyright 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright 1997-1998  Graeme E Moss, and Tommy Thorn

;; Author: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk>
;;         1997-1998 Tommy Thorn <thorn@irisa.fr>
;;         2003      Dave Love <fx@gnu.org>
;; Keywords: faces files PureScript

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'cl-lib)
(require 'purescript-mode)
(require 'font-lock)

(defcustom purescript-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.

This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises with regards to layout."
  :group 'purescript
  :type 'boolean)

(defcustom purescript-font-lock-symbols-alist
  '(("\\" . "λ")
    ("not" . "¬")
    ("->" . "→")
    ("<-" . "←")
    ("=>" . "⇒")
    ("()" . "∅")
    ("==" . "≡")
    ("/=" . "≢")
    (">=" . "≥")
    ("<=" . "≤")
    ("!!" . "‼")
    ("&&" . "∧")
    ("||" . "∨")
    ("sqrt" . "√")
    ("undefined" . "⊥")
    ("pi" . "π")
    ("~>" . "⇝") ;; Omega language
    ;; ("~>" "↝") ;; less desirable
    ("-<" . "↢") ;; Paterson's arrow syntax
    ;; ("-<" "⤙") ;; nicer but uncommon
    ("::" . "∷")
    ("." "∘" ; "○"
     ;; Need a predicate here to distinguish the . used by
     ;; forall <foo> . <bar>.
     purescript-font-lock-dot-is-not-composition)
    ("forall" . "∀"))
  "Alist mapping PureScript symbols to chars.

Each element has the form (STRING . COMPONENTS) or (STRING
COMPONENTS PREDICATE).

STRING is the PureScript symbol.
COMPONENTS is a representation specification suitable as an argument to
`compose-region'.
PREDICATE if present is a function of one argument (the start position
of the symbol) which should return non-nil if this mapping should
be disabled at that position."
  :type '(alist string string)
  :group 'purescript)

(defun purescript-font-lock-dot-is-not-composition (start)
  "Return non-nil if the \".\" at START is not a composition operator.
This is the case if the \".\" is part of a \"forall <tvar> . <type>\"."
  (save-excursion
    (goto-char start)
    (or (re-search-backward "\\<forall\\>[^.\"]*\\="
                            (line-beginning-position) t)
        (not (or
              (string= " " (string (char-after start)))
              (string= " " (string (char-before start))))))))

(defcustom purescript-font-lock-quasi-quote-modes
  `(("hsx" . xml-mode)
    ("hamlet" . xml-mode)
    ("shamlet" . xml-mode)
    ("xmlQQ" . xml-mode)
    ("xml" . xml-mode)
    ("cmd" . shell-mode)
    ("sh_" . shell-mode)
    ("jmacro" . javascript-mode)
    ("jmacroE" . javascript-mode)
    ("r" . ess-mode)
    ("rChan" . ess-mode)
    ("sql" . sql-mode))
  "Mapping from quasi quoter token to fontification mode.

If a quasi quote is seen in PureScript code its contents will have
font faces assigned as if respective mode was enabled."
  :group 'purescript
  :type '(repeat (cons string symbol)))

;;;###autoload
(defface purescript-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight PureScript keywords."
  :group 'purescript)

;;;###autoload
(defface purescript-constructor-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight PureScript constructors."
  :group 'purescript)

;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defface purescript-definition-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight PureScript definitions."
  :group 'purescript)

;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `purescript-definition-face'.
;;;###autoload
(defface purescript-operator-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight PureScript operators."
  :group 'purescript)

;;;###autoload
(defface purescript-pragma-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight PureScript pragmas."
  :group 'purescript)

;;;###autoload
(defface purescript-literate-comment-face
  '((t :inherit font-lock-doc-face))
  "Face with which to fontify literate comments.
Inherit from `default' to avoid fontification of them."
  :group 'purescript)

(defface purescript-quasi-quote-face
  '((t :inherit font-lock-string-face))
  "Generic face for quasiquotes.

Some quote types are fontified according to other mode defined in
`purescript-font-lock-quasi-quote-modes'."
  :group 'purescript)

(defun purescript-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ((eq (char-syntax (char-after start)) ?.) '(?.))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
            (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)

(defun purescript-font-lock-symbols-keywords ()
  (when (and purescript-font-lock-symbols
	     purescript-font-lock-symbols-alist
	     (fboundp 'compose-region))
    `((,(regexp-opt (mapcar 'car purescript-font-lock-symbols-alist) t)
       (0 (purescript-font-lock-compose-symbol ',purescript-font-lock-symbols-alist)
	  ;; In Emacs-21, if the `override' field is nil, the face
	  ;; expressions is only evaluated if the text has currently
	  ;; no face.  So force evaluation by using `keep'.
	  keep)))))

;; The font lock regular expressions.
(defun purescript-font-lock-keywords-create (literate)
  "Create fontification definitions for PureScript scripts.
Returns keywords suitable for `font-lock-keywords'."
  (let* (;; Bird-style literate scripts start a line of code with
         ;; "^>", otherwise a line of code starts with "^".
         (line-prefix (if (eq literate 'bird) "^> ?" "^"))

         (varid "\\b[[:lower:]_][[:alnum:]'_]*\\b")
         ;; We allow ' preceding conids because of DataKinds/PolyKinds
         (conid "\\b'?[[:upper:]][[:alnum:]'_]*\\b")
         (modid (concat "\\b" conid "\\(\\." conid "\\)*\\b"))
         (qvarid (concat modid "\\." varid))
         (qconid (concat modid "\\." conid))
         (sym "\\s.+")

         ;; Reserved identifiers
         (reservedid
          (concat "\\<"
                  ;; `as', `hiding', and `qualified' are part of the import
                  ;; spec syntax, but they are not reserved.
                  ;; `_' can go in here since it has temporary word syntax.
                  ;; (regexp-opt
                  ;;  '("case" "class" "data" "default" "deriving" "do"
                  ;;    "else" "if" "import" "in" "infix" "infixl"
                  ;;    "infixr" "instance" "let" "module" "newtype" "of"
                  ;;    "then" "type" "where" "_") t)
                  "\\(_\\|c\\(ase\\|lass\\)\\|d\\(ata\\|e\\(fault\\|riving\\)\\|o\\)\\|else\\|i\\(mport\\|n\\(fix[lr]?\\|stance\\)\\|[fn]\\)\\|let\\|module\\|mdo\\|newtype\\|of\\|rec\\|proc\\|t\\(hen\\|ype\\)\\|where\\)"
                  "\\>"))

         ;; Top-level declarations
         (topdecl-var
          (concat line-prefix "\\(" varid "\\(?:\\s-*,\\s-*" varid "\\)*" "\\)\\s-*"
                  ;; optionally allow for a single newline after identifier
                  ;; NOTE: not supported for bird-style .lhs files
                  (if (eq literate 'bird) nil "\\([\n]\\s-+\\)?")
                  ;; A toplevel declaration can be followed by a definition
                  ;; (=), a type (::) or (∷), a guard, or a pattern which can
                  ;; either be a variable, a constructor, a parenthesized
                  ;; thingy, or an integer or a string.
                  "\\(" varid "\\|" conid "\\|::\\|∷\\|=\\||\\|\\s(\\|[0-9\"']\\)"))
         (topdecl-var2
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
         (topdecl-bangpat
          (concat line-prefix "\\(" varid "\\)\\s-*!"))
         (topdecl-sym
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
         (topdecl-sym2 (concat line-prefix "(\\(" sym "\\))"))

         keywords)

    (setq keywords
          `(;; NOTICE the ordering below is significant
            ;;
            ("^#.*$" 0 'font-lock-preprocessor-face t)

            ,@(purescript-font-lock-symbols-keywords)

            (,reservedid 1 'purescript-keyword-face)

            ;; Special case for `as', `hiding', `safe' and `qualified', which are
            ;; keywords in import statements but are not otherwise reserved.
            ("\\<import[ \t]+\\(?:\\(safe\\>\\)[ \t]*\\)?\\(?:\\(qualified\\>\\)[ \t]*\\)?\\(?:\"[^\"]*\"[\t ]*\\)?[^ \t\n()]+[ \t]*\\(?:\\(\\<as\\>\\)[ \t]*[^ \t\n()]+[ \t]*\\)?\\(\\<hiding\\>\\)?"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax)
             (3 'purescript-keyword-face nil lax)
             (4 'purescript-keyword-face nil lax))

            ;; Special case for `foreign import'
            ;; keywords in foreign import statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(import\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?\\(?:\\(safe\\|unsafe\\|interruptible\\)[ \t]+\\)?"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax)
             (3 'purescript-keyword-face nil lax)
             (4 'purescript-keyword-face nil lax))

            ;; Special case for `foreign export'
            ;; keywords in foreign export statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(export\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax)
             (3 'purescript-keyword-face nil lax))

            ;; Special case for `type family' and `data family'.
            ;; `family' is only reserved in these contexts.
            ("\\<\\(type\\|data\\)[ \t]+\\(family\\>\\)"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax))

            ;; Toplevel Declarations.
            ;; Place them *before* generic id-and-op highlighting.
            (,topdecl-var  (1 'purescript-definition-face))
            (,topdecl-var2 (2 'purescript-definition-face))
            (,topdecl-bangpat  (1 'purescript-definition-face))
            (,topdecl-sym  (2 (unless (member (match-string 2) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'purescript-definition-face)))
            (,topdecl-sym2 (1 (unless (member (match-string 1) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'purescript-definition-face)))

            ;; These four are debatable...
            ("(\\(,*\\|->\\))" 0 'purescript-constructor-face)
            ("\\[\\]" 0 'purescript-constructor-face)

            (,(concat "`" varid "`") 0 'purescript-operator-face)
            (,(concat "`" conid "`") 0 'purescript-operator-face)
            (,(concat "`" qvarid "`") 0 'purescript-operator-face)
            (,(concat "`" qconid "`") 0 'purescript-operator-face)

            (,qconid 0 'purescript-constructor-face)

            (,conid 0 'purescript-constructor-face)

            (,sym 0 (if (and (eq (char-after (match-beginning 0)) ?:)
                             (not (member (match-string 0) '("::" "∷"))))
                        'purescript-constructor-face
                      'purescript-operator-face))))
    keywords))

(defconst purescript-basic-syntactic-keywords
  '(;; Character constants (since apostrophe can't have string syntax).
    ;; Beware: do not match something like 's-}' or '\n"+' since the first '
    ;; might be inside a comment or a string.
    ;; This still gets fooled with "'"'"'"'"'"', but ... oh well.
    ("\\Sw\\('\\)\\([^\\'\n]\\|\\\\.[^\\'\n \"}]*\\)\\('\\)" (1 "\"") (3 "\""))
    ;; Deal with instances of `--' which don't form a comment
    ("[!#$%&*+./:<=>?@^|~\\]*--[!#$%&*+./:<=>?@^|~\\-]*" (0 (cond ((or (nth 3 (syntax-ppss)) (numberp (nth 4 (syntax-ppss))))
                              ;; There are no such instances inside
                              ;; nestable comments or strings
                              nil)
                             ((string-match "\\`-*\\'" (match-string 0))
                              ;; Sequence of hyphens. Do nothing in
                              ;; case of things like `{---'.
                              nil)
                             ((string-match "\\`[^-]+--.*" (match-string 0))
                              ;; Extra characters before comment starts
                              ".")
                             (t ".")))) ; other symbol sequence

    ;; Implement PureScript Report 'escape' and 'gap' rules. Backslash
    ;; inside of a string is escaping unless it is preceeded by
    ;; another escaping backslash. There can be whitespace between
    ;; those two.
    ;;
    ;; Backslashes outside of string never escape.
    ;;
    ;; Note that (> 0 (skip-syntax-backward ".")) this skips over *escaping*
    ;; backslashes only.
    ("\\\\" (0 (when (save-excursion (and (nth 3 (syntax-ppss))
                                          (goto-char (match-beginning 0))
                                          (skip-syntax-backward "->")
                                          (or (not (eq ?\\ (char-before)))
                                              (> 0 (skip-syntax-backward ".")))))
                  "\\")))

    ;; QuasiQuotes syntax: [quoter| string |], quoter is unqualified
    ;; name, no spaces, string is arbitrary (including newlines),
    ;; finishes at the first occurence of |], no escaping is provided.
    ;;
    ;; The quoter cannot be "e", "t", "d", or "p", since those overlap
    ;; with Template PureScript quotations.
    ;;
    ;; QuasiQuotes opens only when outside of a string or a comment
    ;; and closes only when inside a quasiquote.
    ;;
    ;; (syntax-ppss) returns list with two interesting elements:
    ;; nth 3. non-nil if inside a string. (it is the character that will
    ;;        terminate the string, or t if the string should be terminated
    ;;        by a generic string delimiter.)
    ;; nth 4. nil if outside a comment, t if inside a non-nestable comment,
    ;;        else an integer (the current comment nesting).
    ;;
    ;; Note also that we need to do in in a single pass, hence a regex
    ;; that covers both the opening and the ending of a quasiquote.

    ("\\(\\[[[:alnum:]]+\\)?\\(|\\)\\(]\\)?"
     (2 (save-excursion
          (goto-char (match-beginning 0))
          (if (eq ?\[ (char-after))
              ;; opening case
              (unless (or (nth 3 (syntax-ppss))
                          (nth 4 (syntax-ppss))
                          (member (match-string 1)
                                  '("[e" "[t" "[d" "[p")))
                "\"")
            ;; closing case
            (when (and (eq ?| (nth 3 (syntax-ppss)))
                       (equal "]" (match-string 3))
                       )
              "\"")))))
    ))

(defconst purescript-bird-syntactic-keywords
  (cons '("^[^\n>]"  (0 "<"))
        purescript-basic-syntactic-keywords))

(defconst purescript-latex-syntactic-keywords
  (append
   '(("^\\\\begin{code}\\(\n\\)" 1 "!")
     ;; Note: buffer is widened during font-locking.
     ("\\`\\(.\\|\n\\)" (1 "!"))               ; start comment at buffer start
     ("^\\(\\\\\\)end{code}$" 1 "!"))
   purescript-basic-syntactic-keywords))

(defun purescript-font-lock-fontify-block (lang-mode start end)
  "Fontify a block as LANG-MODE."
  (let ((string (buffer-substring-no-properties start end))
        (modified (buffer-modified-p))
        (org-buffer (current-buffer)) pos next)
    (remove-text-properties start end '(face nil))
    (with-current-buffer
        (get-buffer-create
         (concat " purescript-font-lock-fontify-block:" (symbol-name lang-mode)))
      (delete-region (point-min) (point-max))
      (insert string " ") ;; so there's a final property change
      (unless (eq major-mode lang-mode) (funcall lang-mode))
      (font-lock-ensure)
      (setq pos (point-min))
      (while (setq next (next-single-property-change pos 'face))
        (put-text-property
         (+ start (1- pos)) (1- (+ start next)) 'face
         (get-text-property pos 'face) org-buffer)
        (setq pos next)))
    (add-text-properties
     start end
     '(font-lock-fontified t fontified t font-lock-multiline t))
    (set-buffer-modified-p modified)))

(defun purescript-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for PureScript."
  (cond
   ((nth 3 state)
    (if (equal ?| (nth 3 state))
        ;; find out what kind of QuasiQuote is this
        (let* ((qqname (save-excursion
                        (goto-char (nth 8 state))
                        (skip-syntax-backward "w._")
                        (buffer-substring-no-properties (point) (nth 8 state))))
               (lang-mode (cdr (assoc qqname purescript-font-lock-quasi-quote-modes))))

          (if (and lang-mode
                   (fboundp lang-mode))
              (save-excursion
                ;; find the end of the QuasiQuote
                (parse-partial-sexp (point) (point-max) nil nil state
                                    'syntax-table)
                (purescript-font-lock-fontify-block lang-mode (1+ (nth 8 state)) (1- (point)))
                ;; must return nil here so that it is not fontified again as string
                nil)
            ;; fontify normally as string because lang-mode is not present
            'purescript-quasi-quote-face))
      'font-lock-string-face))
   ;; Else comment.  If it's from syntax table, use default face.
   ((or (eq 'syntax-table (nth 7 state))
        (and (eq purescript-literate 'bird)
             (memq (char-before (nth 8 state)) '(nil ?\n))))
    'purescript-literate-comment-face)
   ;; Detect pragmas. A pragma is enclosed in special comment
   ;; delimeters {-# .. #-}.
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at "{-#")
           (forward-comment 1)
           (goto-char (- (point) 3))
           (looking-at "#-}")))
    'purescript-pragma-face)
   ;; Haddock comment start with either "-- [|^*$]" or "{- ?[|^*$]"
   ;; (note space optional for nested comments and mandatory for
   ;; double dash comments).
   ;;
   ;; Haddock comment will also continue on next line, provided:
   ;; - current line is a double dash haddock comment
   ;; - next line is also double dash comment
   ;; - there is only whitespace between
   ;;
   ;; We recognize double dash haddock comments by property
   ;; 'font-lock-doc-face attached to newline. In case of bounded
   ;; comments newline is outside of comment.
   ((save-excursion
      (goto-char (nth 8 state))
      (or (looking-at "\\(?:{- ?\\|-- \\)[|^*$]")
	  (and (looking-at "--")              ; are we at double dash comment
	       (forward-line -1)              ; this is nil on first line
	       (eq (get-text-property (line-end-position) 'face)
		   'font-lock-doc-face)	      ; is a doc face
	       (forward-line)
	       (skip-syntax-forward "-")      ; see if there is only whitespace
	       (eq (point) (nth 8 state)))))  ; we are back in position
    'font-lock-doc-face)
   (t 'font-lock-comment-face)))

(defconst purescript-font-lock-keywords
  (purescript-font-lock-keywords-create nil)
  "Font lock definitions for non-literate PureScript.")

(defconst purescript-font-lock-bird-literate-keywords
  (purescript-font-lock-keywords-create 'bird)
  "Font lock definitions for Bird-style literate PureScript.")

(defconst purescript-font-lock-latex-literate-keywords
  (purescript-font-lock-keywords-create 'latex)
  "Font lock definitions for LaTeX-style literate PureScript.")

;;;###autoload
(defun purescript-font-lock-choose-keywords ()
  (let ((literate (if (boundp 'purescript-literate) purescript-literate)))
    (cl-case literate
      (bird purescript-font-lock-bird-literate-keywords)
      ((latex tex) purescript-font-lock-latex-literate-keywords)
      (t purescript-font-lock-keywords))))

(defun purescript-font-lock-choose-syntactic-keywords ()
  (let ((literate (if (boundp 'purescript-literate) purescript-literate)))
    (cl-case literate
      (bird purescript-bird-syntactic-keywords)
      ((latex tex) purescript-latex-syntactic-keywords)
      (t purescript-basic-syntactic-keywords))))

(defun purescript-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for PureScript."
  (set (make-local-variable 'font-lock-defaults)
       '(purescript-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . purescript-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . purescript-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t))))

;; The main functions.
(defun turn-on-purescript-font-lock ()
  "Turns on font locking in current buffer for PureScript 1.4 scripts.

Changes the current buffer's `font-lock-defaults', and adds the
following variables:

   `purescript-keyword-face'      for reserved keywords and syntax,
   `purescript-constructor-face'  for data- and type-constructors, class names,
                               and module names,
   `purescript-operator-face'     for symbolic and alphanumeric operators,
   `purescript-default-face'      for ordinary code.

The variables are initialised to the following font lock default faces:

   `purescript-keyword-face'      `font-lock-keyword-face'
   `purescript-constructor-face'  `font-lock-type-face'
   `purescript-operator-face'     `font-lock-function-name-face'
   `purescript-default-face'      <default face>

Two levels of fontification are defined: level one (the default)
and level two (more colour).  The former does not colour operators.
Use the variable `font-lock-maximum-decoration' to choose
non-default levels of fontification.  For example, adding this to
.emacs:

  (setq font-lock-maximum-decoration '((purescript-mode . 2) (t . 0)))

uses level two fontification for `purescript-mode' and default level for
all other modes.  See documentation on this variable for further
details.

To alter an attribute of a face, add a hook.  For example, to change
the foreground colour of comments to brown, add the following line to
.emacs:

  (add-hook 'purescript-font-lock-hook
      (lambda ()
          (set-face-foreground 'purescript-comment-face \"brown\")))

Note that the colours available vary from system to system.  To see
what colours are available on your system, call
`list-colors-display' from emacs.

To turn font locking on for all PureScript buffers, add this to .emacs:

  (add-hook 'purescript-mode-hook 'turn-on-purescript-font-lock)

To turn font locking on for the current buffer, call
`turn-on-purescript-font-lock'.  To turn font locking off in the current
buffer, call `turn-off-purescript-font-lock'.

Bird-style literate PureScript scripts are supported: If the value of
`purescript-literate-bird-style' (automatically set by the PureScript mode
of Moss&Thorn) is non-nil, a Bird-style literate script is assumed.

Invokes `purescript-font-lock-hook' if not nil."
  (purescript-font-lock-defaults-create)
  (run-hooks 'purescript-font-lock-hook)
  (turn-on-font-lock))

(defun turn-off-purescript-font-lock ()
  "Turns off font locking in current buffer."
  (font-lock-mode -1))

(defun purescript-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-substring (point-min) (point-max))))

;; Provide ourselves:

(provide 'purescript-font-lock)

;; Local Variables:
;; coding: utf-8-unix
;; tab-width: 8
;; End:

;;; purescript-font-lock.el ends here

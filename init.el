;;; Defuns
(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms will be
wrapped in a lambda. A list of symbols will expand into a series of add-hook calls.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "add-hook!: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted (eq (car-safe hook) 'quote))
         (hook (if quoted (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms)))
         (forms '()))
    (mapc
     (lambda (f)
       (let ((func (cond ((symbolp f) `(quote ,f))
                         (t `(lambda (&rest _) ,@func-or-forms)))))
         (mapc
          (lambda (h)
            (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
          (-list hook)))) funcs)
    `(progn ,@forms)))



;;; Core
(defconst metro-emacs-dir
  (expand-file-name user-emacs-directory)
  "The path to this emacs.d directory")

(defconst metro-auto-cask
  (expand-file-name
   (format "%s/auto-cask.el" metro-emacs-dir)
  "The path to auto-cask.el"))

(defconst metro-packages-dir
  (expand-file-name
   (format ".cask/%s.%s/elpa" emacs-major-version emacs-minor-version)
   metro-emacs-dir)
  "Where plugins are installed (by cask)")


;;; External package management (Cask)
(require 'auto-cask metro-auto-cask)
(auto-cask/setup metro-emacs-dir)		; locate and setup cask.el

(setq inhibit-startup-message t)

(setq-default
 package--init-file-ensured t			; stop auto-adding (package-initialize) on my behalf
 package-user-dir metro-packages-dir		; packages are installed by cask, in .emacs/.cask dir
 package-enable-at-startup nil			; do not load packages after init.el
 package-archives
 '(("gnu"   . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("org"   . "https://orgmode.org/elpa")))


;;; Completion
(use-package company
  :config
  (global-company-mode +1))


;;; UI stuff
(setq-default
 mode-line-default-help-echo nil ; don't say anything on mode-line mouseover
 indicate-buffer-boundaries nil  ; don't show where buffer starts/ends
 indicate-empty-lines nil        ; don't show empty lines
 fringes-outside-margins t       ; switches order of fringe and margin
 ;; Keep cursors and highlights in current window only
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil
 ;; Disable bidirectional text support for slight performance bonus
 bidi-display-reordering nil
 ;; Remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)

 blink-matching-paren nil ; don't blink--too distracting
 show-paren-delay 0.075
 show-paren-highlight-openparen t
 show-paren-when-point-inside-paren t
 uniquify-buffer-name-style nil
 visible-bell nil
 visible-cursor nil
 x-stretch-cursor t
 use-dialog-box nil             ; always avoid GUI
 redisplay-dont-pause t         ; don't pause display on input
 split-width-threshold nil      ; favor horizontal splits
 show-help-function nil         ; hide :help-echo text
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 ;; Minibuffer resizing
 resize-mini-windows 'grow-only
 max-mini-window-height 0.3
 image-animate-loop t
 ;; Ask for confirmation on exit only if there are real buffers left
 confirm-kill-emacs
 (lambda (_)
   (if (ignore-errors (doom/get-real-buffers))
       (y-or-n-p "››› Quit?")
     t)))

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; hide menu bar
(menu-bar-mode -1)

;; TODO/FIXME/NOTE highlighting in comments
(add-hook! (prog-mode emacs-lisp-mode css-mode)
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOTE\\(?:(.*)\\)?:?\\)\\>"  1 'success prepend))))


(use-package color-theme-solarized
  :init
  (setq color-themes '())			; initialize color-theme symbol
  :config
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized t))
  
(use-package hl-line
  :init (add-hook 'prog-mode-hook 'hl-line-mode)
  :config
  ;; stickiness doesn't play nice with emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;;; Highlight numbers in code source
(use-package highlight-numbers :commands (highlight-numbers-mode))

;;TODO:  doom|nlinum-hl-line
(use-package nlinum
  :commands nlinum-mode
  :preface
  ;(setq linum-format "%3d ")
  (defvar nlinum-format "%4d ")
  :init
  (add-hook!
    (prog-mode markdown-mode conf-mode)
    'nlinum-mode)
  :config
  ;; Calculate line number column width beforehand
  (add-hook! nlinum-mode
    (setq nlinum--width (length (save-excursion (goto-char (point-max))
                                                (format-mode-line "%l"))))))

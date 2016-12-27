(load (concat user-emacs-directory "core/core"))

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

 blink-matching-paren t ; don't blink--too distracting
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

(use-package which-key
  :config
  (which-key-mode))

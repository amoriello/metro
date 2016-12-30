;;; core-evil.el --- the root of all evil

(use-package evil
  :init
  (setq evil-magic t
        evil-want-C-u-scroll t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-interactive-search-highlight 'selected-window
        evil-echo-state nil
        evil-ex-substitute-global t
        evil-insert-skip-empty-lines t
        evil-want-fine-undo nil

        ;; Set cursor colors
        evil-default-cursor (face-attribute 'cursor :foreground nil t)
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  `(,(face-attribute 'warning :foreground nil nil) box)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)

  ;; highlight matching delimiters where it's important
  (defun show-paren-mode-off () (show-paren-mode -1))
  (add-hook 'evil-insert-state-entry-hook   'show-paren-mode)
  (add-hook 'evil-insert-state-exit-hook    'show-paren-mode-off)
  (add-hook 'evil-visual-state-entry-hook   'show-paren-mode)
  (add-hook 'evil-visual-state-exit-hook    'show-paren-mode-off)
  (add-hook 'evil-operator-state-entry-hook 'show-paren-mode)
  (add-hook 'evil-operator-state-exit-hook  'show-paren-mode-off)
  (add-hook 'evil-normal-state-entry-hook   'show-paren-mode-off)
  ;; Disable highlights on insert-mode
  (add-hook 'evil-insert-state-entry-hook 'evil-ex-nohighlight)

  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; evil-anzu is strangely slow on startup. Byte compiling doesn't help.
  ;; We use this to lazy load it instead.
  (defun metro*evil-search (&rest _)
    (require 'evil-anzu)
    (advice-remove 'evil-ex-start-search 'metro*evil-search))
  (advice-add 'evil-ex-start-search :before 'metro*evil-search)

  ;; Reset evil-mode in the messages buffer, because it opens before evil
  ;; normalizes its keymaps, so none of the custom keybindings work in it.
  (add-hook! emacs-startup
    (with-current-buffer "*Messages*"
      (evil-mode -1)
      (evil-mode +1)))

  (mapc (lambda (r) (evil-set-initial-state (car r) (cdr r)))
        '((compilation-mode       . normal)
          (help-mode              . normal)
          (message-mode           . normal)
          (image-mode             . normal)
          (doc-view-mode          . normal)
          (eww-mode               . normal)
          (debugger-mode          . emacs)
          (tabulated-list-mode    . emacs)
          (profile-report-mode    . emacs)
          (Info-mode              . emacs)
          (view-mode              . emacs)
          (comint-mode            . emacs)
          (cider-repl-mode        . emacs)
          (term-mode              . emacs)
          (calendar-mode          . emacs)
          (Man-mode               . emacs)
          (grep-mode              . emacs))))

(use-package evil-anzu
  :defer t
  :config
  (setq anzu-cons-mode-line-p t
        anzu-minimum-input-length 1
        anzu-search-threshold 250))

(provide 'core-evil)
;;; core-evil.el ends here

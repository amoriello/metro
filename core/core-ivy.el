;;; core-ivy.el
;; see defuns/defuns-ivy.el


(use-package ivy
  :init
  (setq ivy-height 14
        ivy-do-completion-in-region nil
        ivy-wrap t
        ;; fontify until EOL
        ivy-format-function 'ivy-format-function-line)

  :config
  (ivy-mode +1)
  (map! :map ivy-minibuffer-map
        [escape] 'keyboard-escape-quit
        "C-r" 'evil-paste-from-register
        "M-v" 'clipboard-yank
        "C-w" 'backward-kill-word
        "C-u" 'backward-kill-sentence
        "C-b" 'backward-word
        "C-f" 'forward-word)

  (after! magit (setq magit-completing-read-function 'ivy-completing-read))

  ;;
  (require 'counsel)

  (advice-add 'counsel-ag-function :override 'metro*counsel-ag-function)
  (define-key counsel-ag-map [backtab] 'metro/counsel-ag-occur)

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

(provide 'core-ivy)
;;; end of core-ivy.el

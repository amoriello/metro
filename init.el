(load (concat user-emacs-directory "core/core"))

(metro core-ui          ; draw me like on of your French editors
       core-editor      ; general editor configuration
       core-company
       core-evil
       core-eval
       core-ivy
       module-python)


(map! "C-j" 'evil-window-down
      "C-k" 'evil-window-up
      "C-h" 'evil-window-left
      "C-l" 'evil-window-right
      "C-x g" 'magit-status)

(use-package magit
  :commands (magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

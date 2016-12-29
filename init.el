(load (concat user-emacs-directory "core/core"))

(metro core-ui		; draw me like on of your French editors
       core-editor)

;;; Completion
(use-package company
  :config
  (global-company-mode +1))

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :commands (magit-status))

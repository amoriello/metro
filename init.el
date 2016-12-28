(load (concat user-emacs-directory "core/core"))

(require 'core-ui)

;;; Completion
(use-package company
  :config
  (global-company-mode +1))

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :commands (magit-status))

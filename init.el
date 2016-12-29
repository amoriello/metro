(load (concat user-emacs-directory "core/core"))

(metro core-ui          ; draw me like on of your French editors
       core-editor      ; general editor configuration
       core-company)

(use-package magit
  :commands (magit-status))

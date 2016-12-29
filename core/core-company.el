;;; core-company.el

(use-package company
  :init
  (setq company-idle-delay 0                    ; show suggestion immediatly
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-dabbrev-downcase nil            ; do not downcase suggestions
        company-dabbrev-ignore-case nil         ; do not ignore case
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t     ; align completion annotation on right
        company-require-match 'never            ; cancel completion by typing non-matching chars
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf)
        company-quickhelp-delay nil
        company-statistics-file (concat metro-temp-dir "/company-stats-cache.el"))
  :config
  (require 'company-capf)

  ;; Rewrites evil-complete to use company-dabbrev
  (setq evil-complete-next-func     'metro/company-evil-complete-next
        evil-complete-previous-func 'metro/company-evil-complete-previous)

  ;; Order result
  (push 'company-sort-by-occurrence company-transformers)
  (global-company-mode +1)

  (require 'company-statistics)
  (company-statistics-mode +1))

(provide 'core-company)
;;; core-company.el ends here

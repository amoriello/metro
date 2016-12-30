;;; core-editor.el

(global-auto-revert-mode 1)    ; revert buffers for changed files

(setq-default
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 ;; Spaces, not tabs
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50
 visual-fill-column-center-text nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; Regions
 shift-select-mode t
 ;; Whitespace
 tabify-regexp "^\t* [ \t]+"
 whitespace-line-column fill-column
 whitespace-space 'underline
 whitespace-style '(face tabs tab-mark
                    trailing indentation lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark 10 [36 10])))

;; Save point across sessions
(require 'saveplace)
(setq-default
 save-place-file (concat metro-temp-dir "/saveplace")
 save-place t)
(when (>= emacs-major-version 25)
  (save-place-mode +1))

;; Keep track of recently opened files
(require 'recentf)
(setq recentf-save-file (concat metro-temp-dir "/recentf")
      recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                        "emacs\\.d/private/cache/.+" "emacs\\.d/workgroups/.+$"
                        "wg-default" "/company-statistics-cache.el$")
      recentf-max-menu-items 0
      recentf-max-saved-items 250
      recentf-auto-cleanup 600
      recentf-filename-handlers '(abbreviate-file-name))
(recentf-mode 1)

;; Let editorconfig handle global whitespace settings
(use-package editorconfig :demand t
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :config (editorconfig-mode +1)
  (push 'doom-mode editorconfig-exclude-modes)
  ;; Show whitespace in tabs indentation mode
  (add-hook! 'editorconfig-custom-hooks
    (if indent-tabs-mode (whitespace-mode +1))))

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally
      ;; no extra frames
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Show which key completion is available after a short delay
(use-package which-key
  :config
  (which-key-mode))

;; Undo tree with C-u
(use-package undo-tree
  :config
  (global-undo-tree-mode t))
(use-package swiper
  :commands (swiper swiper-all))

(provide 'core-editor)
;;; end of core-editor.el

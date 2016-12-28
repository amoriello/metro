;;; core.el --- The heart of the beast
;;
;;; Naming conventions:
;;
;;   metro-...    A public variable/constant or function
;;   metro--...   An internal variable or function (non-interactive)
;;   metro/...    An autoloaded function
;;   metro:...    An ex command
;;   metro|...    A hook
;;   metro*...	  An advising function
;;   ...!	  Macro, shortcut alias or subst defun
;;   @...         Autoloaded interactive lambda macro for keybinds
;;
;;; Autoloaded functions are in {core,modules}/defuns/defuns-*.el

(defconst metro-version "0.0.1"
  "Current version of metro emacs")

(defconst metro-emacs-dir
  (expand-file-name user-emacs-directory)
  "The path to this emacs.d directory")

(defconst metro-core-dir
  (expand-file-name "core" metro-emacs-dir)
  "Where essential files are stored")

(defconst metro-auto-cask
  (expand-file-name
   (format "%s/auto-cask.el" metro-emacs-dir)
  "The path to auto-cask.el"))

(defconst metro-packages-dir
  (expand-file-name
   (format ".cask/%s.%s/elpa" emacs-major-version emacs-minor-version)
   metro-emacs-dir)
  "Where plugins are installed (by cask)")

(setq inhibit-startup-message t)

(setq-default
 package--init-file-ensured t			; stop auto-adding (package-initialize) on my behalf
 package-user-dir metro-packages-dir		; packages are installed by cask, in .emacs/.cask dir
 package-enable-at-startup nil			; do not load packages after init.el
 package-archives
 '(("gnu"   . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("org"   . "https://orgmode.org/elpa")))

;;
;; Bootstrap
;;

(defvar metro--load-path load-path
  "Initial `load-path', used as a base so we don't clobber it on consecutive
reloads.")

(defvar metro-packages '()
  "A list of all installed packages. Filled internally; do not edit it!")

;; Just the bear necessities... ♫
(setq load-path (append (list metro-core-dir) metro--load-path))

;; Populate load-path manually. This way, cask (and `cask-initialize') won't be
;; an internal dependency -- they slow down startup a lot!
(require 'core-defuns)
(let ((paths (eval-when-compile (metro-reload))))
  (setq load-path (car paths)
        metro-paclages (nth 1 paths)))

;; Many functions are lazy-loaded. The autoloads.el file contains info on where
;; to find them if they're called. Tries to generate autoloads.el if one isn't
;; found.
(unless (require 'autoloads nil t)
  (metro-reload-autoloads)
  (unless (require 'autoloads nil t)
    (error "Autoloads weren't generated! Run `make autoloads`")))

;;; External package management (Cask)
;(require 'auto-cask metro-auto-cask)
;(auto-cask/setup metro-emacs-dir)		; locate and setup cask.el


(require 'dash)
(require 's)
(require 'f)

(autoload 'use-package "use-package" "" nil 'macro)


(provide 'core)
;;; core.el ends here

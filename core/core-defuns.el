;;; core-defuns.el

;; Bootstrap macro
(defmacro metro (&rest packages)
  "Bootstrap METRO emacs and initialize PACKAGES"
  `(let (file-name-handler-alist)
     ;; Local settings
     (load "~/.emacs.local.el" t t)
     ;; Bootstrap
     (unless noninteractive
       ,@(mapcar (lambda (pkg)
                   (let ((lib-path (locate-library (symbol-name pkg))))
                     (unless lib-path
                       (error "Initfile not found: %s" pkg))
                     `(require ',pkg ,(file-name-sans-extension lib-path))))
                 packages)
       (when window-system
         (require 'server)
         (unless (server-running-p)
           (server-start)))
       ;; Prevent any auto-displayed text + benchmarking
       (advice-add 'display-startup-echo-area-message :override 'ignore)
       (message ""))
     (setq-default gc-cons-threshold 4388608
                   gc-cons-percentage 0.4)))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load', that supresses warnings
during compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms will be
wrapped in a lambda. A list of symbols will expand into a series of add-hook calls.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "add-hook!: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted (eq (car-safe hook) 'quote))
         (hook (if quoted (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms)))
         (forms '()))
    (mapc
     (lambda (f)
       (let ((func (cond ((symbolp f) `(quote ,f))
                         (t `(lambda (&rest _) ,@func-or-forms)))))
         (mapc
          (lambda (h)
            (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
          (-list hook)))) funcs)
    `(progn ,@forms)))

(defsubst --subdirs (path &optional include-self)
  "Get list of subdirectories in PATH, including PATH is INCLUDE-SELF is
non-nil."
  (let ((result (if include-self (list path) (list))))
    (mapc (lambda (file)
            (when (file-directory-p file)
              (push file result)))
          (ignore-errors (directory-files path t "^[^.]" t)))
    result))

(defun metro-reload ()
  "Reload `load-path' and `custom-theme-load-path', in case you updated cask
while emacs was open!"
  (interactive)
  (let* ((-packages-path (--subdirs metro-packages-dir))
         (-load-path
          (append (list metro-core-dir metro-packages-dir)
                  (--subdirs metro-core-dir t)
                  (--subdirs metro-modules-dir t)
                  -packages-path
                  (--subdirs (expand-file-name (format "../../%s/bootstrap" emacs-version)
                                               metro-packages-dir))
                  metro--load-path)))
    (setq load-path -load-path)
    (if (called-interactively-p 'interactive)
        (message "Reloaded!")
      (list -load-path
            (mapcar '--subdirs -packages-path)))))

(defun metro-reload-autoloads ()
  "Regenerate and reload autoloads.el."
  (interactive)
  (let ((generated-autoload-file (concat metro-core-dir "/autoloads.el")))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file)
      (message "Deleted old autoloads.el"))
    (mapc (lambda (dir)
            (update-directory-autoloads (concat dir "/defuns"))
            (message "Scanned: %s" dir))
          (list metro-core-dir))
    (when (called-interactively-p 'interactive)
      (load "autoloads"))
    (message "Done!")))

(after! evil
  (defalias 'ex! 'evil-ex-define-cmd)

  ;; NOTE evil-mode doesn't read local `evil-ex-commands', and will not
  ;; autocomplete local commands.
  (defun ex-local! (cmd fn)
    "Define a buffer-local ex command."
    (unless (local-variable-p 'evil-ex-commands)
      (setq-local evil-ex-commands (copy-alist evil-ex-commands)))
    (evil-ex-define-cmd cmd fn))

  ;; Register keywords for proper indentation (see `map!')
  (put ':prefix      'lisp-indent-function 'defun)
  (put ':map         'lisp-indent-function 'defun)
  (put ':after       'lisp-indent-function 'defun)
  (put ':when        'lisp-indent-function 'defun)
  (put ':unless      'lisp-indent-function 'defun)
  (put ':leader      'lisp-indent-function 'defun)
  (put ':localleader 'lisp-indent-function 'defun)

  (defmacro map! (&rest rest)
    (let ((i 0)
          (keymaps (if (boundp 'keymaps) keymaps))
          (default-keymaps '((current-global-map)))
          (state-map '(("n" . normal)
                       ("v" . visual)
                       ("i" . insert)
                       ("e" . emacs)
                       ("o" . operator)
                       ("m" . motion)
                       ("r" . replace)))
          (prefix (if (boundp 'prefix) prefix))
          key def states forms)
      (unless keymaps
        (setq keymaps default-keymaps))
      (while rest
        (setq key (pop rest))
        (push
         (reverse
          (cond ((listp key) ; it's a sub exp
                 `(,(macroexpand `(map! ,@key))))

                ((keywordp key)
                 (when (memq key '(:leader :localleader))
                   (push (cond ((eq key :leader)
                                doom-leader)
                               ((eq key :localleader)
                                doom-localleader))
                         rest)
                   (setq key :prefix))
                 (pcase key
                   (:prefix  (setq prefix (concat prefix (kbd (pop rest)))) nil)
                   (:map     (setq keymaps (-list (pop rest))) nil)
                   (:unset  `(,(macroexpand `(map! ,(kbd (pop rest)) nil))))
                   (:after   (prog1 `((after! ,(pop rest)   ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                   (:when    (prog1 `((if ,(pop rest)       ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                   (:unless  (prog1 `((if (not ,(pop rest)) ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                   (otherwise ; might be a state prefix
                    (mapc (lambda (letter)
                            (if (assoc letter state-map)
                                (push (cdr (assoc letter state-map)) states)
                              (user-error "Invalid mode prefix %s in key %s" letter key)))
                          (split-string (substring (symbol-name key) 1) "" t))
                    (unless states
                      (user-error "Unrecognized keyword %s" key)) nil)))

                ;; It's a key-def pair
                ((or (stringp key)
                     (characterp key)
                     (vectorp key))
                 (when (stringp key)
                   (setq key (kbd key)))
                 (when prefix
                   (setq key (cond ((vectorp key) (vconcat prefix key))
                                   (t (concat prefix key)))))
                 (unless (> (length rest) 0)
                   (user-error "Map has no definition for %s" key))
                 (setq def (pop rest))
                 (let (out-forms)
                   (mapc (lambda (keymap)
                           (if states
                               (push `(evil-define-key ',states ,keymap ,key ,def) out-forms)
                             (push `(define-key ,keymap ,key ,def) out-forms)))
                         keymaps)
                   (setq states '())
                   out-forms))
                (t (user-error "Invalid key %s" key))))
         forms)
        (setq i (1+ i)))
      `(progn ,@(apply #'nconc (delete nil (delete (list nil) (reverse forms))))))))

(provide 'core-defuns)
;;; core-defuns.el ends here

;;; core-defuns.el

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
                  -packages-path
                  (--subdirs (expand-file-name (format "../../%s/bootstrap" emacs-version)
                                               metro-packages-dir))
                  metro--load-path)))
    (setq load-path -load-path)
    (if (called-interactively-p 'interactive)
        (message "Reloaded!")
      (list -load-path
            (mapcar '--subdirs -packages-path)))))

(provide 'core-defuns)
;;; core-defuns.el ends here
;;; defuns-ivy.el

;;;###autoload
(defun metro*counsel-ag-function (string base-cmd extra-ag-args)
  "Advice to get rid of the character limit from `counsel-ag-function', which
interferes with my custom :ag ex command `metro:ivy-ag-search'."
  (when (null extra-ag-args)
    (setq extra-ag-args ""))
  (if (< (length string) 1)
      (counsel-more-chars 1)
    (let ((default-directory counsel--git-grep-ir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (let ((ag-cmd (format base-cmd
                            (concat extra-ag-args
                                    " -- "
                                    (shell-quote-argument regex)))))
        (if (file-remote-p default-directory)
            (split-string (shell-command-to-string ag-cmd) "\n" t)
          (counsel--async-command ag-cmd)
          nil)))))

;;;###autoload
(defun metro/counsel-ag-occur ()
  "Invoke the search+replace wgrep buffer on the current ag search results."
  (interactive)
  (require 'wgrep)
  (call-interactively 'ivy-occur))

(provide 'defuns-ivy)
;;; defuns-ivy.el ends here

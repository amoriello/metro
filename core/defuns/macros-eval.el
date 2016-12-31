;;; macros-eval.el

;;;###autoload
(defmacro def-repl! (mode command)
  "Define a REPL for a mode."
  `(push '(,mode . ,command) rtog/mode-repl-alist))

(provide 'macros-eval)
;;; macros-eval.el ends here

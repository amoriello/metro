;;; defuns-company.el

;;;###autoload
(defun metro/company-evil-complete-next (&optional arg)
  "dabbrev wrapper for `evil-complete-next'"
  (call-interactively 'company-dabbrev)
  (if (eq company-candidates-length 1)
      (company-complete)))

;;;###autoload
(defun metro/company-evil-complete-previous (&optional arg)
  "dabbrev wrapper for `evil-complete-previous'"
  (let ((company-selection-wrap-around t))
    (call-interactively 'company-dabbrev)
    (if (eq company-candidates-length 1)
        (company-complete)
      (call-interactively 'company-select-previous))))

;;;###autoload
(defun metro/company-complete-common-or-complete-full ()
  (interactive)
  (when (company-manual-begin)
    (if (eq last-command #'company-complete-common-or-cycle)
        (let ((company-selection-wrap-around t))
          (call-interactively #'company-complete-selection))
      (let ((buffer-mod-tick (buffer-chars-modified-tick)))
        (call-interactively #'company-complete-common)
        (when (= buffer-mod-tick (buffer-chars-modified-tick))
          (call-interactively #'company-complete-selection)
          (call-interactively #'company-complete))))))

(defun metro--company-whole-lines ()
  (split-string
   (replace-regexp-in-string
    "^[\t\s]+" ""
    (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
            (buffer-substring-no-properties (line-end-position) (point-max))))
   "\\(\r\n\\|[\n\r]\\)" t))

;;;###autoload
(defun metro/company-whole-lines (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (unless (bound-and-true-p company-mode) (company-mode))
  (let ((lines (metro--company-whole-lines)))
    (cl-case command
      (interactive (company-begin-backend 'metro/company-whole-lines))
      (prefix (company-grab-line "^[\t\s]*\\(.+\\)" 1))
      (candidates (all-completions arg lines)))))

;;;###autoload
(defun metro/company-dict-or-keywords ()
  (interactive)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively 'company-complete)))

;;;###autoload
(defun metro/company-complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (unless (bound-and-true-p company-mode) (company-mode))
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

(provide 'defuns-company)
;;; defuns-company.el ends here

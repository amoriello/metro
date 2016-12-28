;;; defuns-nlinum.el

;;;###autoload
(defun metro/nlinum-toggle ()
  (interactive)
  (if (bound-and-true-p nlinum-mode)
      (metro|nlinum-disable)
    (metro|nlinum-enable)))

;;;###autoload
(defun metro|nlinum-enable (&rest _)
  (nlinum-mode +1)
  (add-hook 'post-command-hook 'metro|nlinum-hl-line nil t)
  (metro--nlinum-unhl-line))

;;;###autoload
(defun metro|nlinum-disable (&rest _)
  (nlinum-mode -1)
  (remove-hook 'post-command-hook 'metro|nlinum-hl-line t)
  (metro--nlinum-unhl-line))

(defun metro--nlinum-unhl-line ()
  "Unhighlight line number"
  (when metro--hl-nlinum-overlay
    (let* ((disp (get-text-property
                  0 'display (overlay-get metro--hl-nlinum-overlay 'before-string)))
           (str (nth 1 disp)))
      (put-text-property 0 (length str) 'face 'linum str)
      (setq metro--hl-nlinum-overlay nil)
      disp)))

;;;###autoload
(defun metro|nlinum-hl-line (&rest _)
  "Highlight line number"
  (let* ((pbol (line-beginning-position))
         (peol (1+ pbol))
         (max (point-max)))
    ;; Handle EOF case
    (when (>= peol max)
      (setq peol max))
    (jit-lock-fontify-now pbol peol)
    (let ((ov (--first (overlay-get it 'nlinum) (overlays-in pbol peol))))
      (metro--nlinum-unhl-line)
      (when ov
        (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
          (put-text-property 0 (length str) 'face 'metro-nlinum-highlight str)
          (setq metro--hl-nlinum-overlay ov))))))

(provide 'defuns-nlinum)
;;; defuns-nlinum.el ends here

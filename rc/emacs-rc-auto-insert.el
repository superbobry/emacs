;;; emacs-rc-auto-insert.el ---


(add-hook 'find-file-hooks 'auto-insert)

(setq auto-insert-directory (concat root-dir "/auto-insert/")
      auto-insert 'other
      auto-insert-query nil
      auto-insert-alist
      '(("\\.py" . ["insert.py" auto-replace-all])
        ("\\.el" . ["insert.el" auto-replace-all]))
      )

(defun auto-replace-all ()
  (auto-replace-file-name)
  (auto-replace-point))

(defun auto-replace-point ()
  "Moves point to the position of >>><<< token."
  (search-forward ">>><<<" nil t)
  (save-restriction
    (narrow-to-region (match-beginning 0) (match-end 0))
    (replace-match "" t)))

(defun auto-replace-file-name ()
  "Replaces >>>FILE<<< token with buffer file name."
  (save-excursion
    (while (search-forward ">>>FILE<<<" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-nondirectory buffer-file-name) t)
        ))
    ))


;;; emacs-auto-insert.el ends here

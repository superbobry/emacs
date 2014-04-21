;;; init.el ---


(defvar local-dir (file-name-directory user-init-file)
  "The root dir of the Emacs configuration.")

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name local-dir))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(load (local-file-name "core/core-packages"))
(load (local-file-name "core/core-env"))

;; ... roll out the thing!
(load (local-file-name "rc/rc-ui"))
(load (local-file-name "rc/rc-editor"))
(load (local-file-name "rc/rc-languages"))
(load (local-file-name "rc/rc-markup"))
(load (local-file-name "rc/rc-flyspell"))
(load (local-file-name "rc/rc-defuns"))
(load (local-file-name "rc/rc-global-bindings"))

(setq custom-file (local-file-name "custom.el"))
(load custom-file t)


;;; init.el ends here

;;; init.el ---


(defvar local-dir (file-name-directory (or (buffer-file-name)
                                           load-file-name))
  "The root dir of the Emacs configuration.")

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name local-dir))
         (parent-dir (file-name-directory file-name)))
    (unless (or (not parent-dir)
		(file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(load (local-file-name "core/core-packages"))

;; OS X specific settings
(when (eq system-type 'darwin)
  (load (local-file-name "core/core-osx")))

;; ... roll out the thing!
(load (local-file-name "rc/rc-editor"))
(load (local-file-name "rc/rc-ui"))
(load (local-file-name "rc/rc-defuns"))
(load (local-file-name "rc/rc-languages"))
(load (local-file-name "rc/rc-markup"))
(load (local-file-name "rc/rc-flyspell"))
(load (local-file-name "rc/rc-global-bindings"))

(setq custom-file (local-file-name "custom.el"))
(load custom-file t)


;;; init.el ends here

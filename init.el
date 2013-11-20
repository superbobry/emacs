;;; init.el ---


(defvar bobry-dir (file-name-directory (or (buffer-file-name)
                                           load-file-name))
  "The root dir of the Emacs configuration.")

(defvar bobry-snippets-dir (concat bobry-dir "snippets/")
  "This folder stores custom yasnippet bundles.")

(defvar bobry-cache-dir (concat bobry-dir "cache/")
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p bobry-cache-dir)
  (make-directory bobry-cache-dir))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(add-to-list 'load-path bobry-dir)

(load "core/core-packages")

;; OS X specific settings
(when (eq system-type 'darwin)
  (load "core/core-osx"))

;; ... roll out the thing!
(load "rc/rc-editor")
(load "rc/rc-ui")
(load "rc/rc-defuns")
(load "rc/rc-languages")
(load "rc/rc-markup")
(load "rc/rc-flyspell")
(load "rc/rc-global-bindings")

(setq custom-file (expand-file-name "custom.el" bobry-dir))
(load custom-file t)

;;; init.el ends here

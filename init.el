(defvar local-dir user-emacs-directory
  "The root dir of the Emacs configuration.")

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name local-dir))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(setq load-prefer-newer t)

;; Package setup.

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-user-dir (local-file-name "elpa"))
(package-initialize)
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-pin "melpa")

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

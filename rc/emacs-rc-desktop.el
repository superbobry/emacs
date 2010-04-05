;;; emacs-rc-desktop.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(setq-default desktop-missing-file-warning nil)
(setq-default desktop-load-locked-desktop t)
(setq-default desktop-path (quote ("~")))
(setq-default desktop-save t)
(setq-default desktop-save-mode t)
(setq-default save-place t)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

(desktop-read)

;;; emacs-rc-desktop.el ends here
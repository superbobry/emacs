;;; emacs-rc-lua.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'lua-mode)
(require 'flymake-lua)

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-hook 'lua-mode-hook 'run-coding-hook)

;;; emacs-rc-lua.el ends here
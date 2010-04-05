
;;; emacs-rc-python.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'python-mode)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist)
      py-smart-indentation t)

(add-hook 'python-mode-hook 'run-coding-hook)

;;; emacs-rc-python.el ends here
;;; emacs-rc-django.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'django-html-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . django-html-mode))

(add-hook 'django-html-mode-hook
          (lambda ()
            (setq tab-width 2)
            (auto-complete-mode)))

;;; emacs-rc-django.el ends here
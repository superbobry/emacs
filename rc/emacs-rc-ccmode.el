;;; emacs-rc-ccmode.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet


(require 'cc-mode)


(defun my-c-mode-common-hook ()
  (setq
   tab-width 4
   c-basic-offset 4
   indent-tabs-mode nil)
  (c-set-style "java")
  (c-toggle-auto-hungry-state 0)
  (c-toggle-syntactic-indentation 1)
  (c-toggle-electric-state 1)

  (auto-fill-mode 1)

  (local-set-key [return] 'newline-and-indent))

(add-hook 'c-mode-common-hook 'run-coding-hook)


;;; emacs-rc-ccmode.el ends here
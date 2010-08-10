;;; emacs-rc-ccmode.el ---


(require 'cc-mode)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq
              tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil)
             (c-set-style "java")
             (c-toggle-auto-hungry-state 0)
             (c-toggle-syntactic-indentation 1)
             (c-toggle-electric-state 1)

             (local-set-key [return] 'newline-and-indent)))
(add-hook 'c-mode-common-hook 'run-coding-hook)


;;; emacs-rc-ccmode.el ends here

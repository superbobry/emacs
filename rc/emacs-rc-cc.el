;;; emacs-rc-cc.el ---


(require 'cc-mode)

(add-hook 'c-mode-common-hook 'run-coding-hook)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq tab-width 4
                   indent-tabs-mode nil)
             (c-set-style "k&r")
             (local-set-key [return] 'newline-and-indent)))


;;; emacs-rc-cc.el ends here

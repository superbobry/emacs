;;; emacs-rc-cc.el ---


(require 'cc-mode)

(add-hook 'c-mode-common-hook 'run-coding-hook)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-default-style "linux"
                   c-basic-offset 4)
             (c-set-offset 'substatement-open 0)
             (local-set-key [return] 'newline-and-indent)))


;;; emacs-rc-cc.el ends here

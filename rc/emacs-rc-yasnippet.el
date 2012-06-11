;;; emacs-rc-yasnippet.el ---


(require 'yasnippet)
(require 'dropdown-list)

(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/x-prompt
                             yas/ido-prompt))

(yas/global-mode 1)
(yas/reload-all)


;;; emacs-rc-yasnippet.el ends here

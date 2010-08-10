;;; emacs-rc-haskell.el ---


(require 'haskell-mode)
(require 'haskell-ghci)

(add-to-list 'auto-mode-alist  '("\\.hs$" . haskell-mode))

(custom-set-variables
 '(hs-lint-replace-with-suggestions t))

(add-hook 'haskell-mode-hook 'run-coding-hook)
(add-hook 'haskell-mode-hook '(lambda ()
                                (turn-on-haskell-doc-mode)
                                (turn-on-haskell-indent)
                                (turn-on-haskell-ghci)))


;;; emacs-rc-haskell.el ends here

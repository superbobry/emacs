;;; emacs-rc-haskell.el ---


(when (load "haskell-site-file.el" t nil nil)
  (require 'inf-haskell)
  (require 'haskell-checkers)
  (require 'haskell-ghci)

  (add-to-list 'auto-mode-alist  '("\\.hs$" . haskell-mode))

  (add-hook 'haskell-mode-hook 'run-coding-hook)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))


;;; emacs-rc-haskell.el ends here

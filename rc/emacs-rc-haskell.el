;;; emacs-rc-haskell.el ---


(when (load "haskell-site-file.el" t nil nil)
  (require 'inf-haskell)
  (require 'haskell-checkers)
  (require 'haskell-ghci)

  (add-to-list 'auto-mode-alist  '("\\.hs$" . haskell-mode))

  (add-hook 'haskell-mode-hook 'run-coding-hook)
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (setq tab-width 4
                     haskell-indentation-layout-offset 4
                     haskell-indentation-left-offset 4
                     haskell-indentation-ifte-offset 4))))


;;; emacs-rc-haskell.el ends here

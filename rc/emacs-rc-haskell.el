;;; emacs-rc-haskell.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'haskell-mode)
(require 'haskell-ghci)
(require 'hs-lint)

(add-to-list 'auto-mode-alist  '("\\.hs$" . haskell-mode))

(custom-set-variables
 '(hs-lint-replace-with-suggestions t))

(add-hook 'haskell-mode-hook 'run-coding-hook)
(add-hook 'haskell-mode-hook '(lambda ()
                                (turn-on-haskell-doc-mode)
                                (turn-on-haskell-indent)
                                (turn-on-haskell-ghci)
                                (local-set-key "\C-cl" 'hs-lint)))

;;; emacs-rc-haskell.el ends here

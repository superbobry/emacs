;;; emacs-rc-lisp.el ---


;; Elisp
(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; Clojure
(add-hook 'clojure-mode-hook 'run-coding-hook)
(add-hook 'inferior-scheme-mode-hook 'split-window)

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))


;;; emacs-rc-lisp.el ends here

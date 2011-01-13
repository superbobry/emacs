;;; emacs-rc-lisp.el ---


;; Elisp
(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; Scheme
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme" t)
(setq scheme-program-name "guile")

(require 'quack nil t)

(add-hook 'scheme-mode-hook 'run-coding-hook)
(add-hook 'inferior-scheme-mode-hook 'split-window)


;;; emacs-rc-lisp.el ends here

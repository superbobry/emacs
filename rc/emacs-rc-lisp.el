;;; emacs-rc-lisp.el ---


;; Elisp
(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(add-hook 'after-save-hook ;; compile elisp on save
          '(lambda ()
             (when (string-match "\\.el$" (buffer-file-name))
               (byte-compile-file (buffer-file-name)))))


;; Clojure
(add-hook 'clojure-mode-hook 'run-coding-hook)
(add-hook 'inferior-scheme-mode-hook 'split-window)

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))


;;; emacs-rc-lisp.el ends here

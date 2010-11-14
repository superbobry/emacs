;;; emacs-rs-js.el ---


(require 'js2-mode nil t)
(require 'coffee-mode) ;; nice couple :)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(add-hook 'js2-mode-hook 'run-coding-hook)
(add-hook 'js2-mode-hook
          '(lambda ()
             ;; (font-lock-add-keywords
             ;;  'js2-mode `(("\\(function *\\)("
             ;;               (0 (progn (compose-region (match-beginning 1)
             ;;                                         (match-end 1) "ƒ")
             ;;                         nil)))))
             (autopair-mode)))

(setq js2-basic-offset 2
      js2-auto-indent-p 'nil
      js2-bounce-indent-p t
      js2-global-externs '("console")
      js2-electric-keys '(";" "," "*", "{", "("))


;;; emacs-rs-js.el ends here

;;; emacs-rc-markup.el ---


(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\|\\.markdown" . markdown-mode))

(require 'django-html-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . django-html-mode))

(add-hook 'sgml-mode-hook
          (lambda ()
            (setq tab-width 2)
            (auto-complete-mode)))


(when (require 'rainbow-mode nil t)
  (add-hook 'html-mode-hook 'rainbow-turn-on)
  (add-hook 'css-mode-hook 'rainbow-turn-on))

(require 'soy-mode)

(when (require 'po-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
  (autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t))

;;; emacs-rc-markup.el ends here

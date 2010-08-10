;;; emacs-rc-markup.el ---


(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\|\\.markdown" . markdown-mode))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mmm-mode")
(require 'mmm-mako)

(add-to-list 'auto-mode-alist '("\\.mak\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mak\\'" 'mako)

(require 'django-html-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . django-html-mode))

(add-hook 'django-html-mode-hook
          (lambda ()
            (setq tab-width 2)
            (auto-complete-mode)))


(require 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-turn-on)
(add-hook 'css-mode-hook 'rainbow-turn-on)


;;; emacs-rc-markup.el ends here

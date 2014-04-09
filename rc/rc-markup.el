;;; rc-markup.el ---


;; Markdown

(use-package markdown-mode
  :ensure markdown-mode
  :commands markdown-mode
  :mode "\\.md\\|\\.markdown")

;; HTML, CSS

(add-hook 'sgml-mode-hook (lambda () (setq tab-width 2)))

(use-package rainbow-mode
  :ensure rainbow-mode
  :diminish rainbow-mode
  :config (progn
            (add-hook 'html-mode-hook 'rainbow-turn-on)
            (add-hook 'css-mode-hook 'rainbow-turn-on)))

(use-package smartparens
  :ensure smartparens
  :init (sp-with-modes '(html-mode sgml-mode)
          (sp-local-pair "<" ">")))

;; gettext

(when (require 'po-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
  (autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t))

;; LaTeX via AucTeX

(eval-after-load "tex"
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))

(use-package tex-site
  :ensure auctex
  :init (progn
          (when-osx
              (setq TeX-view-program-list '(("Preview" "open %o"))
                    TeX-view-program-selection '((output-pdf "Preview"))))

          (require 'texmathp)

          (setq TeX-auto-save t
                TeX-parse-self t
                TeX-DVI-via-PDFTeX t)

          (add-hook 'LaTeX-mode-hook '(lambda ()
                                        (LaTeX-math-mode 1)
                                        (TeX-fold-mode 1)
                                        (TeX-PDF-mode 1)
                                        (outline-minor-mode 1)))))


;;; rc-markup.el ends here

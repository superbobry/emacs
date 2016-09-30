;;; rc-markup.el ---


;; Markdown

(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :mode "\\.md\\|\\.markdown")

;; HTML, CSS

(add-hook 'sgml-mode-hook (lambda () (setq tab-width 2)))

(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish rainbow-mode
  :config (progn
            (add-hook 'html-mode-hook 'rainbow-turn-on)
            (add-hook 'css-mode-hook 'rainbow-turn-on)))

(use-package smartparens
  :ensure t
  :init (sp-with-modes '(html-mode sgml-mode)
          (sp-local-pair "<" ">")))

;; LaTeX via AucTeX

(eval-after-load "tex"
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))

(use-package tex-site
  :ensure auctex
  :defer t
  :init (progn
          (when-osx
              (setq TeX-view-program-list '(("Preview" "open -a Skim %o"))
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

;;; rc-markup.el ---


;; Markdown

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; HTML, CSS

(add-hook 'sgml-mode-hook (lambda () (setq tab-width 2)))

(use-package smartparens
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

;;; rc-markup.el ---


;; Markdown

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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


;; Org

(use-package org-mode
  :defer t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :init
  (setq org-agenda-files '("~/Dropbox/inbox.org"
                           "~/Dropbox/gtd.org"
                           "~/Dropbox/tickler.org")
        org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "~/Dropbox/inbox.org" "Tasks")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline "~/Dropbox/tickler.org" "Tickler")
           "* %i%? \n %U"))
        org-refile-targets '(("~/Dropbox/gtd.org" :maxlevel . 3)
                             ("~/Dropbox/someday.org" :level . 1)
                             ("~/Dropbox/tickler.org" :maxlevel . 2))
        org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))


;;; rc-markup.el ends here

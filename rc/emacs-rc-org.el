;;; emacs-rc-org.el ---


(require 'org)

(setq org-directory (file-name-as-directory "~/Documents/org")
      org-agenda-files `(,(concat org-directory "gtd.org.gpg"))
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-completion-use-ido t
      org-default-notes-file (concat org-directory "gtd.org")
      org-hide-leading-stars t
      org-fast-tag-selection-single-key 'expert
      org-footnote-auto-adjust t
      org-return-follows-link t
      org-reverse-note-order t
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (type "GIG(g)" "|" "DONE(d!)")
        (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)"))
      org-use-fast-todo-selection t)


;; Capture.
(when (require 'org-capture nil 'noerror)
  (setq org-capture-templates
        '(("t" "Todo" entry (file "gtd.org")
           "* TODO %^{Brief Description} %^g\n%?\nAdded: %U"
           :prepend t))))

(when (require 'deft nil 'noerror)
   (setq
      deft-extension "org"
      deft-directory (concat org-directory "deft")
      deft-text-mode 'org-mode)
   (global-set-key (kbd "<f9>") 'deft))


;;; emacs-rc-org.el ends here

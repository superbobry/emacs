;;; emacs-rc-org.el ---


(require 'org)

(setq org-directory (concat (getenv "HOME") "/Documents/org/"))
(setq org-agenda-files `(,(concat org-directory "gtd.org"))
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-completion-use-ido t
      org-deadline-warning-days 7
      org-default-notes-file (concat org-directory "journal.org")
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
(require 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "gtd.org" "Tasks")
         "* TODO %^{Brief Description} %^g\n%?\nAdded: %U"
         :prepend t)
        ("w" "Word" entry (file+headline "" "Words")
             "** /%^{Word}/ %U :WORD:
%?

Example:")
        ("i" "Idea" entry (file+headline "" "Ideas")
         "** %^{Idea} %U :IDEA:%^g\n%?"
         :prepend t)
        ("b" "Buzz" entry (file+headline "" "Buzz")
         "** %^{Topic} %T %^g \n%i%?\n"
         :prepend t)
      ))

;; Habit.
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-global-properties (quote (("STYLE_ALL" . "habit")))
      org-habit-graph-column 50)

;; Compatibility issues.
(require 'windmove)

(add-hook 'org-shiftup-hook 'windmove-up)
(add-hook 'org-shiftleft-hook 'windmove-left)
(add-hook 'org-shiftdown-hook 'windmove-down)
(add-hook 'org-shiftright-hook 'windmove-right)

;; Weather
(require 'org-google-weather nil t)

;;; emacs-rc-org.el ends here

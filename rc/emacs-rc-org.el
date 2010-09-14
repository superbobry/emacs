;;; emacs-rc-org.el ---


(require 'org)

(setq org-directory (concat (getenv "HOME") "/docs/org/"))
(setq org-agenda-files `(,(concat org-directory "gtd.org"))
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-completion-use-ido t
      org-deadline-warning-days 7
      org-default-notes-file (concat org-directory "journal.org")
      org-hide-leading-stars t
      org-log-done t
      org-fast-tag-selection-single-key 'expert
      org-footnote-auto-adjust t
      org-return-follows-link t
      org-reverse-note-order t
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)"))
      org-use-fast-todo-selection t)

(add-to-list 'org-link-abbrev-alist '("emacswiki" . "http://www.emacswiki.org/cgi-bin/wiki/"))
(add-to-list 'org-link-abbrev-alist '("google" . "http://www.google.com/search?q="))
(add-to-list 'org-link-abbrev-alist '("myspace" . "http://www.myspace.com/"))

;; Remember
(require 'remember)
(require 'org-remember)

(org-remember-insinuate)
(setq remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler)
      org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "gtd.org" "Tasks")
        ("Music" ?m "* %^{Artist} %t :MUSIC:%^g
:PROPERTIES:
  :Artist: %?
  :Release:
  :Genre:
  :Country:
  :Website:
:END:

Comments:\n" nil "Music")
        ("Cd" ?c "* %^{Title} %t :CD:MUSIC:
:PROPERTIES:
  :Artist: %?
  :Release:
  :Label:
  :Genre:
  :Country:
  :Price:
:END:\n\n" nil "CD")
        ("Film" ?f "* %^{Film Title} %t :CINEMA:
:PROPERTIES:
  :Director: %?
  :Year:
  :Genre:
  :Country:
:END:

Comments: \n" nil "Movies")
        ("Word" ?w "** /%^{Word}/ %U :WORD:
%?

Example:
" nil "Words")
        ("Idea" ?i "** %^{Idea} %U :IDEA:%^g \n%?" nil "Ideas")
        ("Buzz" ?b "** %^{Topic} %T %^g \n%i%?\n" nil "Buzz")
      ))

(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Habit
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-global-properties (quote (("STYLE_ALL" . "habit")))
      org-habit-graph-column 50)

;; Compatibility issues.
;; a) windmove
(require 'windmove)

(add-hook 'org-shiftup-hook 'windmove-up)
(add-hook 'org-shiftleft-hook 'windmove-left)
(add-hook 'org-shiftdown-hook 'windmove-down)
(add-hook 'org-shiftright-hook 'windmove-right)

;; b) yasnippet
(when (require 'yasnippet nil t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-set-local 'yas/trigger-key [tab])
              (define-key yas/keymap [tab] 'yas/next-field-group))))

;; Bindings
(global-set-key (kbd "\C-cl") 'org-store-link)
(global-set-key (kbd "\C-ca") 'org-agenda)
(global-set-key (kbd "\C-cr") 'org-remember)

;; Weather
(require 'org-google-weather nil t)


;;; emacs-rc-org.el ends here

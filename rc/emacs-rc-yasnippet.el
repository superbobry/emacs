;;; emacs-rc-yasnippet.el ---


(require 'yasnippet)
(require 'dropdown-list)

(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/x-prompt
                             yas/ido-prompt)
      yas/snippet-dirs (list (concat root-dir "snippets")))

(yas/initialize)


;; use <mode>/template snippet for empty files
(mapc (lambda (hook)
        (add-hook hook '(lambda ()
                          (when (and (bobp) (eobp))
                            (insert "template")
                            (yas/expand)))))
      '(python-mode-hook emacs-lisp-mode))


;;; emacs-rc-yasnippet.el ends here


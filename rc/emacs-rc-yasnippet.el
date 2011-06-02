;;; emacs-rc-yasnippet.el ---


(require 'yasnippet)
(require 'dropdown-list)

(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/x-prompt
                             yas/ido-prompt))
(add-to-list 'yas/snippet-dirs (concat root-dir "snippets"))

(yas/initialize)
(yas/reload-all)


;; use <mode>/template snippet for empty files
(mapc (lambda (hook)
        (add-hook hook '(lambda ()
                          (when (and (bobp) (eobp))
                            (insert "template")
                            (yas/expand)))))
      '(python-mode-hook emacs-lisp-mode))


;;; emacs-rc-yasnippet.el ends here


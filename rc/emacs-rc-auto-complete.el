;;; emacs-rc-autocomplete.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet


(require 'auto-complete)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/dict")

(require 'auto-complete-config)
(ac-config-default)

(setq
 ac-use-fuzzy t
 ac-auto-start 2
 ac-override-local-map nil
 ac-dwim t
 ac-modes
 '(emacs-lisp-mode cc-mode css-mode erlang-mode django-html-mode emacs-lisp-mode
                   espresso-mode haskell-mode lisp-interaction-mode lisp-mode
                   lua-mode python-mode ruby-mode scheme-mode sgml-mode
                   xml-mode org-mode)
 ac-default-sources
 '(ac-source-imenu
   ac-source-words-in-buffer
   ac-source-yasnippet
;   ac-source-semantic
;   ac-source-gtags
;   ac-source-filename
;   ac-source-symbols
   ))

(set-default 'ac-sources ac-default-sources)


(define-key ac-complete-mode-map "\M-/" 'ac-stop)
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)


(add-hook
 'auto-complete-mode-hook
 (lambda ()
   (global-set-key "\M-/" 'ac-start)))


;;; emacs-rc-autocomplete.el ends here
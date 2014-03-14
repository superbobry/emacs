;;; rc-languages.el ---


(defun turn-on-whitespace ()
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun turn-on-linum () (linum-mode t))

(defvar watchwords-regexp
  "\\<\\(NOTE\\|FIXME\\|TODO\\|BUG\\|XXX\\)")

(defun highlight-watchwords ()
  (font-lock-add-keywords
   nil
   `((,watchwords-regexp 1 font-lock-warning-face t))))

(defun annotate-watchwords ()
  "Put fringe marker on watchwords lines in the curent buffer."
  (interactive)
  (remove-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward watchwords-regexp nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

(add-hook 'prog-mode-hook 'turn-on-whitespace)
(add-hook 'prog-mode-hook 'highlight-watchwords)
(add-hook 'prog-mode-hook 'annotate-watchwords)
(add-hook 'prog-mode-hook 'turn-on-linum)

;; Python

(use-package python-mode
  :ensure python-mode
  :commands python-mode
  :config (progn
            (add-hook 'python-mode-hook (lambda ()
                                          (subword-mode +1)
                                          (electric-indent-mode -1)))
            (custom-set-variables
             '(py-start-run-py-shell nil))))

;; Erlang

(require 'em-glob)

(setq erlang-root-dir
      (if (eq system-type 'gnu/linux)
          "/usr/lib/erlang"
        "/usr/local/lib/erlang"))

(defun directory-files-glob (path)
  (directory-files (file-name-directory path)
                   t
                   (eshell-glob-regexp (file-name-nondirectory path))))

(defun directory-any-file-glob (path)
  (car (directory-files-glob path)))

(when (file-exists-p erlang-root-dir)
  (add-to-list 'load-path (concat
                           (file-name-as-directory
                            (directory-any-file-glob
                             (concat erlang-root-dir "/lib/tools-*")))
                           "emacs"))

  (require 'erlang-start))

;; Haskell

(use-package haskell-mode
  :ensure haskell-mode
  :commands haskell-mode
  :init (progn
          (require 'inf-haskell)
          (require 'haskell-checkers)
          (require 'haskell-navigate-imports)

          (bind-keys :map haskell-mode-map
                     ("C-c C-c" . haskell-compile)
                     ("M-[" . haskell-navigate-imports)
                     ("M-]" . haskell-navigate-imports-return))

          (setq haskell-mode-hook nil)
          (add-hook 'haskell-mode-hook
                    '(lambda ()
                       (subword-mode +1)
                       (haskell-doc-mode 1)))))

(use-package hi2
  :ensure hi2
  :init (progn
          (add-hook 'haskell-mode-hook
                    '(lambda ()
                       (setq tab-width 4
                             hi2-layout-offset 4
                             hi2-left-offset 4
                             hi2-ifte-offset 4)

                       ;; (haskell-indent-mode -1)
                       (hi2-mode)))))

(use-package ghc
  :ensure ghc
  :init (progn
          (require 'ghc-core)

          (add-hook 'haskell-mode-hook 'ghc-init)

          (ac-define-source ghc-mod
            '((depends ghc)
              (candidates . (ghc-select-completion-symbol))
              (symbol . "s")
              (cache)))
          (add-to-list 'ac-sources ac-source-ghc-mod)))

(use-package ghci-completion
  :ensure ghci-completion
  :init (progn
          (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)))


;; OCaml

(add-to-list 'load-path
             (concat
              (replace-regexp-in-string
               "\n$" ""
               (shell-command-to-string "opam config var share"))
              "/emacs/site-lisp"))

(let* ((opam-prefix
        (substring (shell-command-to-string "opam config var prefix") 0 -1)))
  (with-temp-buffer
    (insert (shell-command-to-string
             (concat opam-prefix
                     "/bin/ocp-edit-mode emacs -load-global-config")))
    (eval-buffer)))

(require 'ocp-indent)
(require 'ocp-index)
(require 'tuareg)
(setq ocp-indent-config "with_never=true")

(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)
(eval-after-load 'merlin
  '(progn (define-key merlin-mode-map (kbd "C-<up>") nil)
          (define-key merlin-mode-map (kbd "C-<down>") nil)))

;; Coffee

(use-package coffee-mode
  :ensure coffee-mode
  :commands coffee-mode
  :init (progn
          (add-hook 'coffee-mode-hook
                    '(lambda ()
                       (set (make-local-variable 'tab-width) 2)
                       (setq coffee-args-compile '("-c", "--bare")
                             coffee-debug-mode t)

                       ;; Compile '.coffee' files on every save
                       (and (file-exists-p (buffer-file-name))
                            (file-exists-p (coffee-compiled-file-name))
                            (coffee-cos-mode t))))))

;; C, C++

(use-package cc-mode
  :init (progn
          (add-hook 'c-mode-common-hook
                    '(lambda ()
                       (local-set-key (kbd "RET") 'newline-and-indent)
                       (setq c-default-style "linux"
                             c-basic-offset 4)
                       (c-set-offset 'substatement-open 0)))))

;; R

(use-package ess-site
  :ensure ess
  :commands R
  :init (progn
          ;; TODO: why doesn't use-package require it for us?
          (require 'ess-site)

          (setq ess-eval-visibly-p nil
                ess-use-tracebug t
                ess-use-auto-complete t
                ess-help-own-frame 'one
                ess-ask-for-ess-directory nil)
          (setq-default ess-dialect "R")
          (ess-toggle-underscore nil)))

;; Octave

(use-package octave-mode
  :mode "\\.m$")


;; Elisp

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defun recompile-elc-on-save ()
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'recompile-elc-on-save)

(use-package eldoc
  :diminish eldoc-mode)


;; Clojure

(use-package clojure-mode
  :ensure clojure-mode
  :commands clojure-mode
  :mode "\.cljs?$"
  :init (progn
          ;; A quickfix for the missing `put-clojure-indent' function.
          (unless (functionp 'put-clojure-indent)
            (defun put-clojure-indent (sym indent)))

          (add-hook 'inferior-mode-hook 'split-window)))


;;; rc-languages.el ends here

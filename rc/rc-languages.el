;;; rc-languages.el ---


(defun turn-on-whitespace ()
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'turn-on-whitespace)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'pixel-scroll-mode)


;; Python

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :commands python-mode
  :config (progn
            (add-hook 'python-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
            (add-hook 'python-mode-hook
                      (lambda ()
                        ;; See https://github.com/company-mode/company-mode/issues/105
                        ;; for details on this nasty bug.
                        (remove-hook 'completion-at-point-functions
                                     'py-shell-complete t)
                        (remove-hook 'completion-at-point-functions
                                     'py-fast-complete t)
                        (subword-mode +1)
                        (electric-indent-mode -1)

                        (setq-local eldoc-documentation-function nil)))))

(with-eval-after-load 'treemacs
  (defun treemacs-ignore-__pycache__ (file _)
    (string= file "__pycache__"))
  (push #'treemacs-ignore-__pycache__ treemacs-ignored-file-predicates))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package cython-mode
  :commands cython-mode
  :config (add-hook 'cython-mode-hook
                    (lambda ()
                      ;; same bug for cython, damit!
                      (remove-hook 'completion-at-point-functions
                                   'py-shell-complete t))))


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
  :defer t
  :commands haskell-mode
  :config (progn
            (require 'inf-haskell)
            (require 'haskell-compile)
            (require 'haskell-navigate-imports)

            (bind-keys :map haskell-mode-map
                       ("C-c C-c" . haskell-compile)
                       ("M-[" . haskell-navigate-imports)
                       ("M-]" . haskell-navigate-imports-return))

            ;; TODO: hident, maybe?
            (add-hook 'haskell-mode-hook
                      '(lambda ()
                         (subword-mode +1)
                         (haskell-doc-mode 1)))))

(use-package ghci-completion
  :ensure ghci-completion
  :init (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))


;; OCaml

(use-package opam
  :before tuareg
  :config
  (opam-init))

(use-package tuareg
  :commands tuareg-mode
  :hook (tuareg-mode . lsp)
  :config
  (when (executable-find "opam")
    (add-to-list 'load-path
                 (concat
                  (replace-regexp-in-string
                   "\n$" ""
                   (shell-command-to-string "opam var share"))
                  "/emacs/site-lisp"))

    (require 'ocp-indent)
    (setq ocp-indent-config "with_never=true")
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq merlin-command 'opam)))

;; C, C++

(use-package cc-mode
  :init (add-hook 'c-mode-common-hook
                  '(lambda ()
                     (local-set-key (kbd "RET") 'newline-and-indent)
                     (setq c-default-style "linux"
                           c-basic-offset 4)
                     (c-set-offset 'substatement-open 0))))

(use-package cmake-mode
  :defer t)


;; R

(use-package ess-site
  :ensure ess
  :defer t
  :commands R
  :init (progn
          (setq ess-eval-visibly-p nil
                ess-use-tracebug t
                ess-use-auto-complete t
                ess-help-own-frame 'one
                ess-ask-for-ess-directory nil)
          (setq-default ess-dialect "R")
          (setq ess-insert-assign nil)))


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


;;; rc-languages.el ends here

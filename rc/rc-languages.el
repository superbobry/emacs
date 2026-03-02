;;; rc-languages.el ---


(defun turn-on-whitespace ()
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'turn-on-whitespace)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


;; Python

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :commands python-ts-mode
  :config (progn
            (add-hook 'python-ts-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
            (add-hook 'python-ts-mode-hook
                      (lambda ()
                        ;; See https://github.com/company-mode/company-mode/issues/105
                        ;; for details on this nasty bug.
                        (remove-hook 'completion-at-point-functions
                                     'py-shell-complete t)
                        (remove-hook 'completion-at-point-functions
                                     'py-fast-complete t)
                        (subword-mode +1)
                        (electric-indent-mode -1)

                        (setq-local eldoc-documentation-function nil)))
            (add-hook 'python-ts-mode-hook 'eglot-ensure)))

(with-eval-after-load 'treemacs
  (defun treemacs-ignore-__pycache__ (file _)
    (string= file "__pycache__"))
  (push #'treemacs-ignore-__pycache__ treemacs-ignored-file-predicates))


(use-package cython-mode
  :commands cython-mode
  :config (add-hook 'cython-mode-hook
                    (lambda ()
                      ;; same bug for cython, damit!
                      (remove-hook 'completion-at-point-functions
                                   'py-shell-complete t))))


;; Erlang

(require 'em-glob nil t)

(setq erlang-root-dir
      (if (eq system-type 'gnu/linux)
          "/usr/lib/erlang"
        "/usr/local/lib/erlang"))

(defun directory-files-glob (path)
  (directory-files (file-name-directory path)
                   t
                   (if (fboundp 'eshell-glob-regexp)
                       (eshell-glob-regexp (file-name-nondirectory path))
                     (file-name-nondirectory path))))

(defun directory-any-file-glob (path)
  (car-safe (directory-files-glob path)))

(when (file-exists-p erlang-root-dir)
  (let ((erlang-tools-dir (directory-any-file-glob (concat erlang-root-dir "/lib/tools-*"))))
    (when erlang-tools-dir
      (add-to-list 'load-path (concat (file-name-as-directory erlang-tools-dir) "emacs"))
      (require 'erlang-start nil t))))


;; Haskell

(use-package haskell-mode
  :defer t
  :commands haskell-mode
  :config (progn
            (require 'inf-haskell nil t)
            (require 'haskell-compile nil t)
            (require 'haskell-navigate-imports nil t)

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
  :config
  (opam-init))

(use-package tuareg
  :commands tuareg-mode
  :config
  (when (executable-find "opam")
    (add-to-list 'load-path
                 (concat
                  (replace-regexp-in-string
                   "\n$" ""
                   (shell-command-to-string "opam var share"))
                  "/emacs/site-lisp"))

    (require 'ocp-indent nil t)
    (require 'dune nil t)
    (setq ocp-indent-config "with_never=true")
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq merlin-command 'opam)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend))
    (add-hook 'merlin-mode-hook 'company-mode)))

;; C, C++

(use-package c-ts-mode
  :mode ("\\.c\\'" . c-ts-mode)
  :mode ("\\.cpp\\'" . c++-ts-mode)
  :mode ("\\.h\\'" . c-or-c++-ts-mode)
  :mode ("\\.hpp\\'" . c-or-c++-ts-mode)
  :init (add-hook 'c-ts-base-mode-hook
                  '(lambda ()
                     (local-set-key (kbd "RET") 'newline-and-indent)
                     (setq-default c-ts-mode-indent-style 'linux
                                   c-ts-mode-indent-offset 4)
                     (eglot-ensure))))

(use-package cmake-mode
  :defer t)

;; Bazel
(use-package bazel
  :mode ("\\(BUILD\\|WORKSPACE\\|\\.\\(BUILD\\|bazel\\|bzl\\)\\)\\'" . bazel-mode))


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

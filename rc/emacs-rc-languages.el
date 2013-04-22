;;; emacs-rc-languages.el ---


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
(add-hook 'prog-mode-hook 'subword-mode)

;; Python

(when (require 'python-mode nil t)
  (custom-set-variables
   '(py-start-run-py-shell nil)))

;; Erlang

(require 'em-glob)

(setq erlang-root-dir
      (if (eq system-type 'gnu/linux)
          "/usr/lib/erlang"
        "/usr/local/lib/erlang"))

(add-to-list 'ac-modes 'erlang-mode)

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

  (when (and (require 'erlang-start nil t)
             (require 'erlang-flymake nil t))
    (erlang-flymake-only-on-save)))

;; Haskell

(when (load "haskell-site-file.el" t nil nil)
  (require 'inf-haskell)
  (require 'haskell-checkers)
  (require 'haskell-ghci)
  (require 'haskell-navigate-imports)

  (add-to-list 'auto-mode-alist  '("\\.hs$" . haskell-mode))

  (setq haskell-mode-hook nil)
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (haskell-indentation-mode 1)
               (haskell-doc-mode 1)

               (local-set-key (kbd "M-[") 'haskell-navigate-imports)
               (local-set-key (kbd "M-]") 'haskell-navigate-imports-return)

               (setq tab-width 4
                     haskell-indentation-layout-offset 4
                     haskell-indentation-left-offset 4
                     haskell-indentation-ifte-offset 4))))

;; OCaml

(when (require 'typerex nil t)
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
  (add-to-list 'interpreter-mode-alist '("ocamlrun" . typerex-mode))
  (add-to-list 'interpreter-mode-alist '("ocaml" . typerex-mode))
  (autoload 'typerex-mode "typerex" "Major mode for editing Caml code" t)

  (setq typerex-in-indent 0
        ocp-syntax-coloring "tuareg_like"
        ocp-auto-complete t))

;; Coffee

(when (require 'coffee-mode nil t)
  (add-hook 'coffee-mode-hook
            '(lambda ()
               (set (make-local-variable 'tab-width) 2)
               (setq coffee-args-compile '("-c", "--bare")
                     coffee-debug-mode t)

               ;; Compile '.coffee' files on every save
               (and (file-exists-p (buffer-file-name))
                    (file-exists-p (coffee-compiled-file-name))
                    (coffee-cos-mode t)))))

;; C, C++

(require 'cc-mode)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-default-style "linux"
                   c-basic-offset 4)
             (c-set-offset 'substatement-open 0)
             (local-set-key [return] 'newline-and-indent)))

;; R

(when (require 'ess-site nil t)
  (setq ess-eval-visibly-p nil
        ess-use-tracebug t
        ess-use-auto-complete t
        ess-help-own-frame 'one
        ess-ask-for-ess-directory nil)
  (setq-default ess-dialect "R")
  (ess-toggle-underscore nil))

;; Octave

(add-to-list 'auto-mode-alist  '("\\.m$" . octave-mode))
(add-to-list 'ac-modes 'octave-mode)

;; Elisp

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(add-hook 'after-save-hook ;; compile elisp on save
          '(lambda ()
             (when (string-match "\\.el$" (buffer-file-name))
               (byte-compile-file (buffer-file-name)))))

;; Clojure

(when (require 'clojure-mode nil t)
  ;; A quickfix for the missing `put-clojure-indent' function.
  (unless (functionp 'put-clojure-indent)
    (defun put-clojure-indent (sym indent)))

  (add-hook 'inferior-scheme-mode-hook 'split-window))

(add-to-list 'auto-mode-alist '("\.cljs?$" . clojure-mode))


;;; emacs-rc-languages.el ends here

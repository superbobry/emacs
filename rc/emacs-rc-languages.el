;;; emacs-rc-languages.el ---


(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

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

(add-hook 'coding-hook 'turn-on-whitespace)
(add-hook 'coding-hook 'highlight-watchwords)
(add-hook 'coding-hook 'annotate-watchwords)
(add-hook 'coding-hook 'turn-on-linum)

(defun run-coding-hook ()
  (interactive)
  (run-hooks 'coding-hook))


;; Python

(when (require 'python-mode nil t)
  (add-hook 'python-mode-hook 'run-coding-hook))

;; Erlang

(require 'em-glob)

(setq erlang-root-dir
      (if (eq system-type 'gnu/linux)
          "/usr/lib/erlang"
        "~/.homebrew/lib/erlang"))

(add-to-list 'ac-modes 'erlang-mode)

(defun directory-files-glob (path)
  (directory-files (file-name-directory path)
                   t
                   (eshell-glob-regexp (file-name-nondirectory path))))

(defun directory-any-file-glob (path)
  (car (directory-files-glob path)))

(add-to-list 'load-path (concat
                         (file-name-as-directory
                          (directory-any-file-glob
                           (concat erlang-root-dir "/lib/tools-*")))
                         "emacs"))

(when (and (require 'erlang-start nil t)
           (require 'erlang-flymake nil t))
  (add-hook 'erlang-mode-hook 'run-coding-hook)

  ;; FIXME(Sergei): add 'eflymake' to path?
  (unless (eq system-type 'darwin)
    (erlang-flymake-only-on-save)))

;; Haskell

(when (load "haskell-site-file.el" t nil nil)
  (require 'inf-haskell)
  (require 'haskell-checkers)
  (require 'haskell-ghci)

  (add-to-list 'auto-mode-alist  '("\\.hs$" . haskell-mode))

  (setq haskell-mode-hook nil)
  (add-hook 'haskell-mode-hook 'run-coding-hook)
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (haskell-indentation-mode 1)
               (haskell-doc-mode 1)
               (setq tab-width 4
                     haskell-indentation-layout-offset 4
                     haskell-indentation-left-offset 4
                     haskell-indentation-ifte-offset 4))))

;; OCaml

(when (require 'typerex nil t)
  (add-hook 'typerex-mode-hook 'run-coding-hook)
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
  (autoload 'typerex-mode "typerex" "Major mode for editing OCaml code" t)
  (setq typerex-in-indent 0
        ocp-syntax-coloring "tuareg_like"
        ocp-auto-complete nil))

;; Coffee

(when (require 'coffee-mode nil t)
  (add-hook 'coffee-mode-hook 'run-coding-hook)
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

(add-hook 'c-mode-common-hook 'run-coding-hook)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-default-style "linux"
                   c-basic-offset 4)
             (c-set-offset 'substatement-open 0)
             (local-set-key [return] 'newline-and-indent)))

;; R

(when (require 'ess-site nil t)
  (setq ess-eval-visibly-p nil
        ess-ask-for-ess-directory nil))

;; Octave

(add-hook 'octave-mode-hook 'run-coding-hook)

(add-to-list 'auto-mode-alist  '("\\.m$" . octave-mode))
(add-to-list 'ac-modes 'octave-mode)

;; Elisp

(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
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

  (add-hook 'clojure-mode-hook 'run-coding-hook)
  (add-hook 'inferior-scheme-mode-hook 'split-window))

(add-to-list 'auto-mode-alist '("\.cljs?$" . clojure-mode))


;;; emacs-rc-languages.el ends here

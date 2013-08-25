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

(require 'python-mode)
(add-hook 'python-mode-hook (lambda ()
                              (subword-mode +1)
                              (electric-indent-mode -1)))
(custom-set-variables
 '(py-start-run-py-shell nil))

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

(eval-after-load 'haskell-mode
  '(progn
     (require 'inf-haskell)
     (require 'haskell-checkers)
     (require 'haskell-navigate-imports)
     (require 'ghc-core)
     (require 'ghc)

     (setq haskell-mode-hook nil)
     (add-hook 'haskell-mode-hook
               '(lambda ()
                  (subword-mode +1)

                  (haskell-indent-mode -1)
                  (turn-on-hi2)
                  (haskell-doc-mode 1)

                  (local-set-key (kbd "M-[") 'haskell-navigate-imports)
                  (local-set-key (kbd "M-]") 'haskell-navigate-imports-return)

                  (ghc-init)

                  (setq tab-width 4
                        hi2-layout-offset 4
                        hi2-left-offset 4
                        hi2-ifte-offset 4)))))

;; OCaml

(let* ((opam-prefix
        (substring (shell-command-to-string "opam config var prefix") 0 -1)))
  (load-file
   (concat opam-prefix "/share/typerex/ocp-indent/ocp-indent.el"))
  (load-file
    (concat opam-prefix "/share/typerex/ocp-index/ocp-index.el"))

  (setq ocp-index-path (concat opam-prefix "/bin/ocp-index")
        ocp-indent-path (concat opam-prefix "/bin/ocp-indent")
        ocp-indent-config "with_never=true")

  (with-temp-buffer
    (insert (shell-command-to-string
             (concat opam-prefix
                     "/bin/ocp-edit-mode emacs -load-global-config")))
    (eval-buffer)))


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

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


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

(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

;; Clojure

(when (require 'clojure-mode nil t)
  ;; A quickfix for the missing `put-clojure-indent' function.
  (unless (functionp 'put-clojure-indent)
    (defun put-clojure-indent (sym indent)))

  (add-hook 'inferior-scheme-mode-hook 'split-window))

(add-to-list 'auto-mode-alist '("\.cljs?$" . clojure-mode))


;;; rc-languages.el ends here

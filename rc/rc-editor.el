;;; rc-editor.el ---

;; Note
;; ----
;; A lot of stuff here was stolen from the wonderful 'Emacs Prelude'
;; project, available at 'https://github.com/bbatsov/prelude'.

(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width 4           ;; but maintain correct appearance
              default-directory "~"
              fill-column 80)

;; All things UTF-8.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; delete the selection with a keypress
(delete-selection-mode t)

;; take care of the whitespace
(require 'whitespace)
(setq whitespace-style '(face trailing lines-tail
                              space-before-tab
                              indentation space-after-tab)
      whitespace-line-column 80)

;; nice things
(setq require-final-newline t     ;; end files with a newline
      mouse-yank-at-point t       ;; yank at cursor, NOT at mouse position
      kill-whole-line t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t     ;; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ;; don't muck with special buffers

;; activate it for all buffers
(save-place-mode 1)
(require 'saveplace)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (local-file-name "cache/saveplace"))

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (local-file-name "cache/savehist"))
(savehist-mode t)

(require 'desktop)
(setq-default desktop-missing-file-warning nil
              desktop-load-locked-desktop t
              desktop-restore-eager 0
              desktop-path `(,(local-file-name "cache"))
              desktop-save t)
(desktop-save-mode t)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 30)
                (shell-command-history    . 50)
                tags-file-name
                register-alist))
      desktop-locals-to-save nil)
(desktop-read)

;; save recent files
(require 'recentf)
(setq recentf-save-file (local-file-name "cache/recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

(require 'epa-file)
(epa-file-enable)
(setq auth-sources '("~/.authinfo.gpg"))

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(use-package super-save
  :config
  (super-save-mode +1))

;; diminish keeps the modeline tidy
(use-package diminish)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; subtle highlighting of matching parens (global-mode)
(use-package smartparens-config
  :ensure smartparens
  :defer t
  :init (progn
          (smartparens-global-mode 1)
          (set-face-foreground 'show-paren-match "white")))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)

;; highlight the current line
(global-hl-line-mode +1)

(use-package volatile-highlights
  :config (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;; tramp, for sudo access
(require 'tramp)
;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *"
      tramp-default-method "ssh")

;; enabled auto-fill mode in text-mode and all related modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; load flycheck
(use-package flycheck
  :defer t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

;; load fast spellchecker
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :custom (jinx-languages "en_GB")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; load auto-complete
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-no-match 'separator)
  :init
  (setq global-corfu-modes '((not text-mode) t))
  (global-corfu-mode))

(use-package eglot
  :defer t
  :commands (eglot eglot-ensure)
  :config
  ;; Eglot uses standard Emacs keybindings, but we can customize the prefix.
  (define-key eglot-mode-map (kbd "C-c l") 'eglot-command-map)
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyrefly" "lsp")))
  (add-to-list 'eglot-server-programs '(c-ts-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd"))))

;; (use-package treemacs
;;   :pin melpa-stable
;;   :hook (emacs-startup . treemacs)
;;   :init
;;   (setq treemacs-position 'right)
;;   :config
;;   (treemacs-follow-mode t)
;;   (treemacs-filewatch-mode t)
;;   (treemacs-add-and-display-current-project-exclusively)
;;   (treemacs-hide-gitignored-files-mode t)
;;   (treemacs-toggle-show-dotfiles))

;; (use-package treemacs-nerd-icons
;;   :pin melpa-stable
;;   :config
;;   (treemacs-load-theme "nerd-icons"))

;; (use-package treemacs-magit
;;   :pin melpa-stable
;;   :after (treemacs magit))

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; clean up obsolete buffers automatically
(require 'midnight)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(require 're-builder)
(setq reb-re-syntax 'string)

;; (use-package golden-ratio
;;   :diminish golden-ratio-mode
;;   :init (progn (golden-ratio-mode)
;;                (setq golden-ratio-auto-scale t)))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  ;; Allow fuzzy matching (like flx)
  (setq orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

(use-package consult
  :bind
  (("C-c i" . consult-imenu)
   ("C-x b" . consult-buffer)
   ("C-x f" . consult-recent-file)
   ("M-y" . consult-yank-pop)
   ("C-s" . consult-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any)))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-switch-commands #'magit-project-status))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; sensible undo
(use-package undo-fu
  :commands undo-tree-visualize
  :bind (("C-S--" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

;; my git
(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (setq async-bytecomp-allowed-packages nil))

(use-package git-timemachine
  :defer t
  :commands git-timemachine)

;; incremental searching
;; (use-package anzu
;;   :diminish anzu-mode
;;   :init (global-anzu-mode +1))

;; view large files easily
(use-package vlf-setup
  :ensure vlf
  :defer t
  :commands vlf)

;; semantic region expansion
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :bind (("C-S-<up>" . move-text-up)
         ("C-S-<down>" . move-text-down)))

(use-package which-key
  :config (which-key-mode))

(use-package multiple-cursors
  :bind (("M-s-<down>" . mc/mark-next-like-this)
         ("M-s-<up>" . mc/mark-previous-like-this)))

(use-package gptel
  :bind (("C-c RET" . gptel-send))
  :config
  (setq gptel-model 'gemini-3.1-pro-preview
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (lambda ()
                               (let ((secret (plist-get (car (auth-source-search :host "gemini" :user "apikey" :require '(:secret))) :secret)))
                                 (if (functionp secret) (funcall secret) secret)))
                        :stream t)))

;;; rc-editor.el ends here

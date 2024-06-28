;;; rc-editor.el ---

;; Note
;; ----
;; A lot of stuff here was stolen from the wonderful 'Emacs Prelude'
;; project, available at 'https://github.com/bbatsov/prelude'.

(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width 4           ;; but maintain correct appearance
              case-fold-search t    ;; case INsensitive search
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
(setq next-line-add-newlines nil    ;; don't add new lines when scrolling down
      require-final-newline t     ;; end files with a newline
      mouse-yank-at-point t       ;; yank at cursor, NOT at mouse position
      kill-whole-line t
      bidi-display-reordering nil)

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
(setq-default save-place t)
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
(require 'diminish)

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
      tramp-default-method "ssh"
      tramp-temp-buffer-file-name (local-file-name "cache/tramp"))

;; enabled auto-fill mode in text-mode and all related modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; load flycheck
(use-package flycheck
  :defer t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

;; load auto-complete
(use-package company
  :defer t
  :init (global-company-mode)
  :custom
  (company-minumum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-enable-imenu)
  :init
  (setq lsp-enable-snippet nil

        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-headerline-breadcrumb-icons-enable nil)
  :config
  (lsp-enable-which-key-integration t)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :custom
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-imenu t)
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package treemacs
  :hook (emacs-startup . treemacs)
  :init
  (setq treemacs-position 'right)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-add-and-display-current-project-exclusively)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-toggle-show-dotfiles))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

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

(use-package ivy
  :diminish ivy-mode
  :bind
  (:map ivy-minibuffer-map
        ("C-w" . ivy-yank-word))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-use-selectable-prompt t
          ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                  (counsel-ag . ivy--regex-plus)
                                  (t . ivy--regex-fuzzy)))

    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))

    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read))))

;; Enhance fuzzy matching
(use-package flx
  :before ivy)

;; Enhance M-x
(use-package amx
  :after ivy)

(use-package counsel
  :diminish counsel-mode
  :bind
  (("C-c i" . counsel-imenu)
   ("C-h a" . counsel-apropos)
   ("C-x f" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x))
  :config (setq counsel-find-file-at-point t))

(use-package swiper
  :bind ("C-s" . swiper)
  :config (progn
            (setq swiper-action-recenter t)))

(use-package helpful
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-create-missing-test-files t
        projectile-switch-project-action #'projectile-commander))

(use-package counsel-projectile
  :init (counsel-projectile-mode))

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

;; better grep-find (consider helm-ag)
;; (use-package ag
;;   :defer t
;;   :commands ag
;;   :init (setq ag-highlight-search t
;;               ag-reuse-window t))

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

;;; rc-editor.el ends here

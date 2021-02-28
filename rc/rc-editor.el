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

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; diminish keeps the modeline tidy
(require 'diminish)

(use-package auto-package-update
  :ensure t
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
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)

;; highlight the current line
(global-hl-line-mode +1)

(use-package volatile-highlights
  :ensure t
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
  :ensure t
  :defer t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

;; load auto-complete
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode))

(use-package company-box
  :ensure t
  :pin "melpa"
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :after lsp-mode
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :after lsp-mode
  :ensure t
  :commands company-lsp)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(require 're-builder)
(setq reb-re-syntax 'string)

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init (progn (golden-ratio-mode)
               (setq golden-ratio-auto-scale t)))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (:map ivy-minibuffer-map
        ("C-w" . ivy-yank-word))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-use-selectable-prompt t
          ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))

    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read))

    ;; Enhance fuzzy matching
    (use-package flx
      :ensure t)

    ;; Enhance M-x
    (use-package amx
      :ensure t)))

(use-package counsel
  :ensure t
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
  :ensure t
  :bind ("C-s" . swiper)
  :config (progn
            (setq swiper-action-recenter t)))

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package projectile
  :ensure t
  :init
  (progn
    (setq projectile-completion-system 'ivy
          projectile-create-missing-test-files t
          projectile-switch-project-action #'projectile-commander)
    (projectile-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
  :diminish projectile-mode)

(use-package counsel-projectile
  :ensure t
  :init (progn
          (counsel-projectile-mode)))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; sensible undo
(use-package undo-tree
  :ensure t
  :commands undo-tree-visualize
  :bind ("C-S-z" . undo-tree-redo)
  :config (progn
            (global-undo-tree-mode)
            (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff t
                  undo-tree-auto-save-history t)

            (defadvice undo-tree-make-history-save-file-name
                (after undo-tree activate)
              (setq ad-return-value (concat ad-return-value ".gz")))

            (custom-set-variables
             '(undo-tree-history-directory-alist
               (quote (("." . "~/.emacs.d/undo/"))))))
  :diminish undo-tree-mode)

;; my git
(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-c g" . magit-status)
  :config (progn
            (setq async-bytecomp-allowed-packages nil)))

(use-package git-timemachine
  :ensure t
  :defer t
  :commands git-timemachine)

;; incremental searching
;; (use-package anzu
;;   :ensure t
;;   :diminish anzu-mode
;;   :init (global-anzu-mode +1))

;; better grep-find (consider helm-ag)
;; (use-package ag
;;   :ensure t
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
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :ensure t
  :bind (("C-S-<up>" . move-text-up)
         ("C-S-<down>" . move-text-down)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package define-word
  :ensure t)

(use-package pretty-mode
  :ensure t
  :config
  (progn
    (global-pretty-mode t)

    (pretty-deactivate-groups
     '(:equality :ordering :ordering-double :ordering-triple
                 :arrows :arrows-twoheaded :punctuation
                 :logic :sets :arithmetic-nary))

    (pretty-activate-groups
     '(:sub-and-superscripts :greek))))

;;; rc-editor.el ends here

;;; rc-editor.el ---

;; Note
;; ----
;; A lot of stuff here was stolen from the wonderful 'Emacs Prelude'
;; project, available at 'https://github.com/bbatsov/prelude'.

(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width 4           ;; but maintain correct appearance
              case-fold-search t    ;; case INsensitive search
              default-directory "~"
              fill-column 100)

;; delete the selection with a keypress
(delete-selection-mode t)

;; take care of the whitespace
(require 'whitespace)
(setq whitespace-style '(face trailing lines-tail
                              space-before-tab
                              indentation space-after-tab)
      whitespace-line-column 80)

;; nice things
(setq next-line-add-newlines nil  ;; don't add new lines when scrolling down
      require-final-newline t     ;; end files with a newline
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

;; subtle highlighting of matching parens (global-mode)
(require 'smartparens)
(require 'smartparens-config)
(show-smartparens-global-mode t)
(smartparens-global-mode 1)

;; highlight the current line
(global-hl-line-mode +1)

(use-package volatile-highlights
  :ensure volatile-highlights
  :init (progn
          (volatile-highlights-mode t)
          (diminish 'volatile-highlights-mode)))

;; tramp, for sudo access
(require 'tramp)
;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *"
      tramp-default-method "ssh"
      tramp-temp-buffer-file-name (local-file-name "cache/tramp"))

;; ido-mode
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)
(ido-mode 'both)
(ido-everywhere t)
(ido-ubiquitous-mode +1)
(flx-ido-mode +1)
(setq flx-ido-use-faces nil)
(setq ido-case-fold t                    ;; be case-insensitive
      ido-confirm-unique-completion nil  ;; wait for RET, even with unique completion
      ido-enable-flex-matching nil       ;; not, too smart, baby ...
      ido-enable-prefix nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-max-prospects 10
      ido-use-faces t
      ido-save-directory-list-file (local-file-name "cache/ido.last")
      ido-default-file-method 'selected-window)

;; enabled auto-fill mode in text-mode and all related modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; load yasnippet
(require 'yasnippet)
(require 'dropdown-list)
(add-to-list 'yas-snippet-dirs (local-file-name "snippets"))
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt))

;; load flycheck
(use-package flycheck
  :ensure flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

;; load auto-complete
(use-package company
  :ensure company
  :init (global-company-mode))

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (local-file-name "cache/eshel"))

;; better splits
(use-package golden-ratio
  :ensure golden-ratio
  :init (progn
          (golden-ratio-mode)
          (diminish 'golden-ratio-mode)))

;; smex, remember recently and most frequently used commands
(use-package smex
  :ensure smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :init (progn
          (setq smex-save-file (local-file-name "cache/.smex-items"))
          (smex-initialize)))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; sensible undo
(use-package undo-tree
  :ensure undo-tree
  :commands undo-tree
  :init (progn
          (global-undo-tree-mode 1)
          (diminish 'undo-tree-mode)
          (defalias 'redo 'undo-tree-redo)))

;; my git
(use-package magit
  :ensure magit
  :commands magit-status
  :init (setq magit-emacsclient-executable nil))

;; incremental searching
(use-package anzu
  :ensure anzu
  :init (global-anzu-mode +1))

;; better grep-find
(use-package ag
  :ensure ag
  :commands ag
  :config (setq ag-highlight-search t
                ag-reuse-window t))

;; fix me already!
(use-package fixmee
  :ensure fixmee
  :init (progn
          (global-fixmee-mode 1)
          (diminish 'fixmee-mode)))

;; view large files easily
(use-package vlf
  :ensure vlf
  :commands vlf
  :init (require 'vlf-integrate))


;;; rc-editor.el ends here

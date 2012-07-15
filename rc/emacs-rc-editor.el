;;; emacs-rc-editor.el ---

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
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column fill-column)

;; nice things
(setq  next-line-add-newlines nil  ;; don't add new lines when scrolling down
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

;; smart pairing for all
(electric-pair-mode t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t     ;; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ;; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(setq save-place-file (concat bobry-cache-dir "saveplace"))

;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

;; savehist keeps track of some history
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat bobry-cache-dir "savehist"))
(savehist-mode t)
(require 'savehist)

(require 'desktop)
(setq-default desktop-missing-file-warning nil
              desktop-load-locked-desktop t
              desktop-path `(,(concat bobry-dir "cache/"))
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
(setq recentf-save-file (concat bobry-cache-dir "recentf")
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

;; show-paren-mode: subtle highlighting of matching parens (global-mode)
(require 'paren)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)

;; highlight the current line
(global-hl-line-mode +1)

(when (require 'volatile-highlights nil t)
  (volatile-highlights-mode t))

;; tramp, for sudo access
(require 'tramp)
;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *"
      tramp-default-method "ssh"
      tramp-temp-buffer-file-name (concat bobry-cache-dir "tramp/"))

;; ido-mode
(require 'ido)
(ido-mode 'both)
(ido-everywhere t)
(setq ido-case-fold t                  ;; be case-insensitive
      ido-confirm-unique-completion t  ;; wait for RET, even with unique completion
      ido-enable-flex-matching t       ;; not, too smart, baby ...
      ido-enable-prefix nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (concat bobry-cache-dir "ido.last")
      ido-default-file-method 'selected-window)

;; auto-completion in minibuffer
(icomplete-mode +1)
(setq icomplete-prospects-height 1     ;; don't spam my minibuffer
      icomplete-compute-delay 0)       ;; don't wait

;; enabled auto-fill mode in text-mode and all related modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; load yasnippet
(when (and (require 'yasnippet nil t)
           (require 'dropdown-list))
  (add-to-list 'yas/snippet-dirs bobry-snippets-dir)
  (yas/global-mode 1)
  (setq yas/prompt-functions '(yas/dropdown-prompt
                               yas/x-prompt
                               yas/ido-prompt)))

;; load auto-complete
(when (and (require 'auto-complete nil t)
           (require 'auto-complete-config nil t))
  (setq ac-comphist-file (concat bobry-cache-dir "ac-comphist.dat")
        ac-candidate-limit 20
        ac-ignore-case nil)
  (global-auto-complete-mode))

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
(setq eshell-directory-name (concat bobry-cache-dir "eshell/"))


;;; emacs-rc-editor.el ends here

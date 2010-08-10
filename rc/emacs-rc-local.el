;;; emacs-rc-local.el ---


(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-input-method nil)

(if (not (eq window-system 'nil))
    (normal-erase-is-backspace-mode))

(setq
 auto-save-interval 512 ;; autosave every 512 keyboard inputs
 auto-save-list-file-prefix nil
 color-theme-is-global t
 echo-keystrokes 0.01 ;; see what you type
 inhibit-startup-message t ; don't show annoing startup msg
 initial-scratch-message nil
 kill-whole-line t ;; delete line in one stage
 mouse-yank-at-point t ;; paste at cursor, NOT at mouse pointer position
 next-line-add-newlines nil ;; don't add new lines when scrolling down
 require-final-newline t ;; end files with a newline
 safe-local-variable-values '((encoding . utf-8) (prompt-to-byte-compile))
 scroll-margin 0                   ;; do smooth scrolling, ...
 scroll-conservatively 100000      ;; ... the defaults ...
 scroll-up-aggressively 0          ;; ... are very ...
 scroll-down-aggressively 0        ;; ... annoying
 scroll-preserve-screen-position t ;; preserve screen pos with C-v/M-v
 whitespace-style '(trailing lines space-before-tab
                             indentation space-after-tab)
 whitespace-line-column 100
 x-select-enable-clipboard t)

(setq-default major-mode 'org-mode
              tab-width 4
              case-fold-search t ;; case INsensitive search
              indent-tabs-mode nil ;; do not use tabs for indentation
              fill-column 80) ;; number of chars in line

;; Backups
(setq make-backup-files t ;; do make backups
      backup-by-copying t ;; and copy them here
      backup-directory-alist '(("." . (concat root-dir "/cache/backups")))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

(setq auto-save-list-file-prefix
  (concat root-dir "/cache/auto-save-list/.saves-"))

(require 'saveplace)
(setq save-place-file (concat root-dir "/cache/saveplace"))
(setq-default save-place t) ;; activate it for all buffers

(require 'recentf)
(setq recentf-save-file (concat root-dir "/cache/recentf")
      recentf-max-saved-items 100
      recentf-max-menu-items 10)
(recentf-mode t)

(require 'desktop)
(setq-default desktop-missing-file-warning nil
              desktop-load-locked-desktop t
              desktop-path (quote (concat root-dir "/cache/"))
              desktop-save t)
(desktop-save-mode t)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (shell-command-history    . 50)
                tags-file-name
                register-alist))
      desktop-locals-to-save nil)
(desktop-read)

(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(require 'autopair)
(autopair-global-mode)

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(require 'sr-speedbar nil 'noerror)
(setq
 sr-speedbar-width-x 20
 sr-speedbar-right-side t)

(require 'tramp)
;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *")
(setq
  tramp-default-method "ssh"
  tramp-persistency-file-name (concat root-dir "/cache/tramp"))

(require 'color-theme)
(require 'color-theme-desert)
(color-theme-initialize)
(color-theme-desert)

(mouse-avoidance-mode 'cat-and-mouse)

(global-linum-mode 0)   ;; no line number unless i say so ...
(global-hl-line-mode 0) ;; ... and no line highlighting too

(setq
 cursor-in-non-selected-windows nil
 use-dialog-box nil)

;; Modeline
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

;; Minibuffer
(setq
  enable-recursive-minibuffers nil ;;  allow mb cmds in the mb
  max-mini-window-height .25       ;;  max 2 lines
  minibuffer-scroll-window nil
  resize-mini-windows nil)

(icomplete-mode t)                 ;; completion in minibuffer
(setq
  icomplete-prospects-height 1     ;; don't spam my minibuffer
  icomplete-compute-delay 0)       ;; don't wait
(require 'icomplete+ nil 'noerror) ;; drew adams' extras

(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

(global-font-lock-mode t)
(transient-mark-mode t)

(fringe-mode '(0 . 0)) ;; nah, i don't like fringe

(fset 'yes-or-no-p 'y-or-n-p)


;;; emacs-rc-local.el ends here

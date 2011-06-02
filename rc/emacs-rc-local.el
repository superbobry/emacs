;;; emacs-rc-local.el ---

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(if (null window-system)
    (normal-erase-is-backspace-mode))

(setq
 auto-save-interval 512            ;; autosave every 512 keyboard inputs
 auto-save-list-file-prefix nil
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "/usr/bin/chromium"
 color-theme-is-global t
 echo-keystrokes 0.01              ;; see what you type
 inhibit-startup-message t         ;; don't show annoing startup msg
 initial-scratch-message nil
 kill-whole-line t                 ;; delete line in one stage
 mouse-yank-at-point t             ;; paste at cursor, NOT at mouse pointer position
 next-line-add-newlines nil        ;; don't add new lines when scrolling down
 require-final-newline t           ;; end files with a newline
 safe-local-variable-values '((encoding . utf-8) (prompt-to-byte-compile))
 scroll-margin 0                   ;; do smooth scrolling, ...
 scroll-conservatively 100000      ;; ... the defaults ...
 scroll-up-aggressively 0          ;; ... are very ...
 scroll-down-aggressively 0        ;; ... annoying
 scroll-preserve-screen-position t ;; preserve screen pos with C-v / M-v
 whitespace-style '(trailing lines space-before-tab
                             indentation space-after-tab)
 whitespace-line-column 100
 x-select-enable-clipboard t)

(setq-default default-directory "~"
              tab-width 4
              case-fold-search t   ;; case INsensitive search
              indent-tabs-mode nil ;; do not use tabs for indentation
              fill-column 80)      ;; number of chars in line

;; Backups
(setq make-backup-files t ;; do make backups
      backup-by-copying t ;; and copy them here
      backup-directory-alist `(("." . ,(concat root-dir "cache/backups")))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

(setq auto-save-list-file-prefix
  (concat root-dir "cache/auto-save-list/.saves-"))

(require 'saveplace)
(setq save-place-file (concat root-dir "cache/saveplace"))
(setq-default save-place t) ;; activate it for all buffers

(require 'longlines) ;; oh please break those long lines for me ...

(require 'recentf)
(setq recentf-save-file (concat root-dir "cache/recentf")
      recentf-max-saved-items 100
      recentf-max-menu-items 10)
(recentf-mode t)

(require 'desktop)
(setq-default desktop-missing-file-warning nil
              desktop-load-locked-desktop t
              desktop-path `(,(concat root-dir "cache/"))
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

;; scratch buffers for the active mode with two key strokes!
(autoload 'scratch "scratch" nil t)

(require 'tramp)
;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *")
(setq
  tramp-default-method "ssh"
  tramp-persistency-file-name (concat root-dir "cache/tramp"))

(set-frame-font "Monaco-13")

(mouse-avoidance-mode 'cat-and-mouse)

(global-linum-mode 0)   ;; no line number unless i say so ...
(global-hl-line-mode 1) ;; ... and please add line highlighting ...
(blink-cursor-mode -1)  ;; ... and cut that blinking out, okay?

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

(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

(global-font-lock-mode t)
(transient-mark-mode t)

;; stop prmopting me, allright?
;; a) y is yes and n is no
(fset 'yes-or-no-p 'y-or-n-p)
;; b) i don't care if the process is running
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))


(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)


(when (and (require 'auto-complete nil t)
           (require 'auto-complete-config nil t))
  (setq ac-comphist-file (concat root-dir "cache/ac-comphist.dat")
        ac-candidate-limit 20
        ac-ignore-case nil)
  (global-auto-complete-mode))


;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)


;;; emacs-rc-local.el ends here

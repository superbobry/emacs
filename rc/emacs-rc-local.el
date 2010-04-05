;;; emacs-rc-local.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(if (not (eq window-system 'nil))
    (normal-erase-is-backspace-mode))

(setq
 auto-save-interval 512 ;; autosave every 512 keyboard inputs
 auto-save-list-file-prefix nil
 browse-url-browser-function 'browse-url-firefox
 browse-url-firefox-new-window-is-tab t
 browse-url-firefox-program "/usr/bin/chromium"
 browse-url-new-window-flag t
 color-theme-is-global t
 comint-completion-addsuffix t ;; Insert space/slash after completion
 completion-ignore-case t
 default-major-mode 'text-mode
 default-tab-width 4
 delete-old-versions t ;; delete excess backup versions
 echo-keystrokes 0.01 ;; see what you type
 inhibit-startup-message t ; don't show annoing startup msg
 initial-scratch-message nil
 kill-whole-line t ;; delete line in one stage
 make-backup-files nil ;; NO annoing backups
 max-specpdl-size 2048 ; bigger stack size
 mouse-yank-at-point t ;; paste at cursor, NOT at mouse pointer position
 next-line-add-newlines nil ;; don't add new lines when scrolling down
 safe-local-variable-values '((encoding . utf-8) (prompt-to-byte-compile))
 shift-select-mode nil
 split-width-threshold nil
 whitespace-style '(trailing lines space-before-tab
                             indentation space-after-tab)
 whitespace-line-column 100
 x-select-enable-clipboard t)


(setq-default
 case-fold-search t ;; case INsensitive search
 indent-tabs-mode nil ;; do not use tabs for indentation
 fill-column 80 ;; number of chars in line
 save-place t)

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 60)

(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(require 'autopair)
(autopair-global-mode)

(require 'color-theme)
(color-theme-initialize)

(load-file (concat root-dir "packages/themes/color-theme-twilight.el"))
(color-theme-twilight)

(mouse-avoidance-mode 'cat-and-mouse)

(global-linum-mode 0) ; line numbers
(global-hl-line-mode 0)

(setq
 cursor-in-non-selected-windows nil
 use-dialog-box nil)

(when window-system
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode t)
(show-paren-mode t)
(global-font-lock-mode t)
(transient-mark-mode t)

(display-time-mode t)
(setq
 display-time-24hr-format t
 display-time-day-and-date nil
 display-time-default-load-average nil)

(fset 'yes-or-no-p 'y-or-n-p)

;;; emacs-rc-local.el ends here
;;; rc-ui.el ---


(when (window-system)
  (x-focus-frame nil)
  (let ((font-name "Monaco-14"))
    (when (find-font (font-spec :name font-name))
      (set-frame-font font-name))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; disable startup screen and *scratch* message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; prevent screen tearing
(setq redisplay-dont-pause t)

;; speedup cursor movemenet
;; https://emacs.stackexchange.com/a/28746
(setq auto-window-vscroll nil)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(mouse-avoidance-mode 'cat-and-mouse)

(setq ring-bell-function 'ignore)

(global-linum-mode 0)   ;; no line number unless i say so ...
(blink-cursor-mode -1)  ;; ... and cut out that blinking, okay?

(setq cursor-in-non-selected-windows nil
      use-dialog-box nil)

;; stop prompting me, allright?
;; a) y is yes and n is no
(fset 'yes-or-no-p 'y-or-n-p)
;; b) i don't care if the process is running
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

(use-package smart-mode-line
  :ensure t
  :init (setq sml/theme 'respectful
              sml/no-confirm-load-theme t))

(use-package zenburn-theme
  :ensure t
  :config (progn
            (load-theme 'zenburn t)
            (sml/setup)))


;;; rc-ui.el ends here

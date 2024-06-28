;;; rc-ui.el ---


(when (window-system)
  (x-focus-frame nil)
  (let ((font-name "Fira Code-15"))
    (when (find-font (font-spec :name font-name))
      (set-frame-font font-name))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode t)

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

(global-display-line-numbers-mode 0)
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

(use-package nerd-icons)

(use-package fira-code-mode
  :hook prog-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :config
  (fira-code-mode-set-font))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (load-theme 'doom-one-light t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :config (progn
            (add-hook 'change-major-mode-hook #'turn-on-solaire-mode)))

;;; rc-ui.el ends here

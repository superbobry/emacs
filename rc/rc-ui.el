;;; rc-ui.el ---


(when (window-system)
  (x-focus-frame nil)
  (let ((font-name "Fira Code-15"))
    (when (find-font (font-spec :name font-name))
      (set-frame-font font-name))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-group-by nil)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("M-s-<left>" . centaur-tabs-backward)
  ("M-s-<right>" . centaur-tabs-forward))

;; disable startup screen and *scratch* message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
      
(pixel-scroll-precision-mode 1)

;; mode line settings
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
(setq use-short-answers t)
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

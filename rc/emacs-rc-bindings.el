;;; emacs-rc-bindings.el ---


;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; File finding
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))
(when (fboundp 'recentf-mode)
  (global-set-key (kbd "M-<f12>") 'recentf-open-files)
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file))

;; Window switching
(windmove-default-keybindings)

;; Editing
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-c C-d") 'duplicate-line)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-S") 'toggle-input-method)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-<tab>") 'textmate-shift-right)
(global-set-key (kbd "C-S-<iso-lefttab>") 'textmate-shift-left)
;; FIXME: screws minibuffer prompt ...
;; (global-set-key (kbd "<return>") 'newline-maybe-indent)

;; Misc
(global-set-key (kbd "C-x c") 'kill-daemon)

(global-unset-key (kbd "C-x h"))
(global-unset-key (kbd "C-c h"))
(global-set-key (kbd "C-x h h") 'hs-hide-block)
(global-set-key (kbd "C-x h s") 'hs-show-block)

(when (featurep 'magit)
  (global-set-key (kbd "C-c g") 'magit-status))

(when (featurep 'ahg)
  (global-set-key (kbd "C-c h") 'ahg-status))

(when (require 'scratch 'nil t)
  (global-set-key (kbd "C-c s") 'scratch))

(when (featurep 'nav)
  (global-set-key (kbd "C-c n") 'nav-toggle))

(when (featurep 'bm)
  (global-set-key (kbd "C-x r t") 'bm-toggle)
  (global-set-key (kbd "C-x r n") 'bm-next)
  (global-set-key (kbd "C-x r p") 'bm-prev)
  (global-set-key (kbd "C-x r a") 'bm-show-all))

(when (featurep 'org)
  (global-set-key (kbd "\C-ca") 'org-agenda)
  (global-set-key (kbd "\C-cc") 'org-capture)
  (global-set-key (kbd "\C-cl") 'org-store-link))


;;; emacs-rc-bindings.el ends here

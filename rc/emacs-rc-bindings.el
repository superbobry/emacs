;;; emacs-rc-bindings.el ---


;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-normal-size)

;; File finding
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(when (fboundp 'recentf-mode)
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file))

;; Editing
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)

;; Misc
(when (featurep 'org)
  (global-set-key (kbd "\C-ca") 'org-agenda)
  (global-set-key (kbd "\C-cc") 'org-capture)
  (global-set-key (kbd "\C-cl") 'org-store-link))

(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x C-g") 'magit-status))

(when (fboundp 'scratch)
  (global-set-key (kbd "C-x C-\\") 'scratch))

(when (featurep 'multi-term)
  (global-set-key (kbd "M-`") 'multi-term-dedicated-toggle))


;;; emacs-rc-bindings.el ends here

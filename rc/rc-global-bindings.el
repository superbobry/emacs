;;; rc-global-bindings.el ---


;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; File finding
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; Editing
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

;; Misc
(global-set-key (kbd "C-c g") 'magit-status)

;;; rc-global-bindings.el ends here

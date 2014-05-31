;;; rc-global-bindings.el ---


;; File finding
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; Editing
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-S-<up>") 'move-line-up)
(global-set-key (kbd "C-S-<down>") 'move-line-down)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)


;;; rc-global-bindings.el ends here

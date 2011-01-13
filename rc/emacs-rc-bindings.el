;;; emacs-rc-bindings.el ---


;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; File finding
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(when (fboundp 'recentf-mode)
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file))

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
(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)

(when (featurep 'org)
  (global-set-key (kbd "\C-ca") 'org-agenda)
  (global-set-key (kbd "\C-cc") 'org-capture)
  (global-set-key (kbd "\C-cl") 'org-store-link))


;;; emacs-rc-bindings.el ends here

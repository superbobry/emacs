;;; emacs-rc-textmate.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'textmate)
(textmate-mode t)

(define-key *textmate-mode-map* (kbd "M-<RET>") 'textmate-next-line)
(define-key *textmate-mode-map* (kbd "C-c C-t") 'textmate-clear-cache)
(define-key *textmate-mode-map* (kbd "C-c C-a") 'align)
(define-key *textmate-mode-map* (kbd "C-<tab>") 'textmate-shift-right)
(define-key *textmate-mode-map* (kbd "C-S-<iso-lefttab>") 'textmate-shift-left)
(define-key *textmate-mode-map* (kbd "C-c C-k") 'comment-or-uncomment-region-or-line)
(define-key *textmate-mode-map* (kbd "M-t") 'transpose-words)

;;; emacs-rc-textmate.el ends here
;;; emacs-rc-math.el ---


(add-to-list 'auto-mode-alist  '("\\.m$" . octave-mode))

(add-to-list 'ac-modes 'octave-mode)

(add-hook 'octave-mode-hook 'run-coding-hook)


;;; emacs-rc-math.el ends here

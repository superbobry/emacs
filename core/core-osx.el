;;; core-osx.el ---


(custom-set-variables '(shell-file-name "zsh"))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Make sure Emacs uses Mountain Lion fullscreen feature, only avail. since
;; Emacs 24.3.
;; (when (functionp 'ns-toggle-fullscreen)
;;   (ns-toggle-fullscreen))


;;; core-osx.el ends here

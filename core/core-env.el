;;; core-env.el ---


;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)


(custom-set-variables '(shell-file-name "zsh"))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(defmacro when-osx (&rest body)
  (declare (indent 1) (debug t))
  (list 'if (eq system-type 'darwin) (cons 'progn body)))

;; Make sure Emacs uses Mountain Lion fullscreen feature, only avail. since
;; Emacs 24.3.
;; (when (functionp 'ns-toggle-fullscreen)
;;   (ns-toggle-fullscreen))


;;; core-env.el ends here

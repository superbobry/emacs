;;; emacs-rc-osx.el ---


(custom-set-variables '(shell-file-name "zsh"))

(when (not (getenv "TERM_PROGRAM"))
  (setenv "PATH"
          (shell-command-to-string
           "source $HOME/.zshrc > /dev/null && printf $PATH"))
  (add-to-list 'exec-path "~/.homebrew/bin"))

(add-to-list 'load-path "~/.homebrew/share/emacs/site-lisp")

;; Make sure Emacs uses Mountain Lion fullscreen feature, only avail. since
;; Emacs 24.3.
;; (when (functionp 'ns-toggle-fullscreen)
;;   (ns-toggle-fullscreen))


;;; emacs-rc-osx.el ends here

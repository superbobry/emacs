;;; emacs-rc-osx.el ---


(custom-set-variables
 '(shell-file-name "~/.homebrew/bin/zsh"))

(when (not (getenv "TERM_PROGRAM"))
  (setenv "PATH"
          (shell-command-to-string
           "source $HOME/.zshrc > /dev/null && printf $PATH"))
  (add-to-list 'exec-path "~/.homebrew/bin"))

(add-to-list 'load-path "~/.homebrew/share/emacs/site-lisp")


;;; emacs-rc-osx.el ends here

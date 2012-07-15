;;; emacs-rc-osx.el ---


(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))


;;; emacs-rc-osx.el ends here

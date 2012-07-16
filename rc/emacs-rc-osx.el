;;; emacs-rc-osx.el ---


(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))

(setq erlang-flymake-command "~/.homebrew/bin/erlc")


;;; emacs-rc-osx.el ends here

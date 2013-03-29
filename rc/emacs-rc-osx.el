;;; emacs-rc-osx.el ---


(custom-set-variables '(shell-file-name "zsh"))

(when (not (getenv "TERM_PROGRAM"))
  (setenv "PATH"
          (shell-command-to-string
           "source $HOME/.zshrc > /dev/null && printf $PATH"))
  (add-to-list 'exec-path "/usr/local/bin"))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(with-temp-buffer
  (insert
   (shell-command-to-string "ocp-edit-mode emacs -load-global-config"))
  (eval-buffer))

(let* ((opam-prefix
        (substring (shell-command-to-string "opam config var prefix") 0 -1)))
  (load-file
   (concat opam-prefix "/share/typerex/ocp-indent/ocp-indent.el"))

  (setq ocp-indent-path (concat opam-prefix "/bin/ocp-indent")
        ocp-indent-config "with_never=true"))



;; Make sure Emacs uses Mountain Lion fullscreen feature, only avail. since
;; Emacs 24.3.
;; (when (functionp 'ns-toggle-fullscreen)
;;   (ns-toggle-fullscreen))


;;; emacs-rc-osx.el ends here

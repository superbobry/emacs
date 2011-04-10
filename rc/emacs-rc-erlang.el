;;; emacs-rc-erlang.el ---


(setq erlang-root-dir "/usr/lib/erlang")

(add-to-list 'load-path (concat erlang-root-dir "/lib/tools-2.6.6.2/emacs"))
(add-to-list 'exec-path (concat erlang-root-dir "/bin"))

(when (and (require 'erlang-start nil t)
           (require 'erlang-flymake nil t))
  (erlang-flymake-only-on-save))


;;; emacs-rc-erlang.el ends here

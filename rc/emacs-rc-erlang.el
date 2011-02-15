;;; emacs-rc-erlang.el ---


(setq erlang-root-dir "/usr/lib/erlang")

(add-to-list 'load-path (concat erlang-root-dir "/lib/tools-2.6.6.1/emacs"))
(add-to-list 'exec-path (concat erlang-root-dir "/bin"))

(require 'erlang-start)
(require 'erlang-flymake)

(erlang-flymake-only-on-save)


;;; emacs-rc-erlang.el ends here

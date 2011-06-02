;;; emacs-rc-erlang.el ---


(when (and (require 'erlang-start nil t)
           (require 'erlang-flymake nil t))
  (erlang-flymake-only-on-save))


;;; emacs-rc-erlang.el ends here

;;; emacs-rc-erlang.el ---


(let* ((emacs-version "2.6.4")
       (tools-path
        (concat "/usr/local/lib/erlang/lib/tools-" emacs-version "/emacs")))
  (when (file-exists-p tools-path)
    (setq load-path (cons tools-path load-path))
    (setq erlang-root-dir "/usr/local/lib/erlang")
    (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
    (require 'erlang-start)
    (defvar inferior-erlang-prompt-timeout t)))

(add-hook 'erlang-mode-hook 'yas/minor-mode)
(add-hook 'erlang-mode-hook 'run-coding-hook)


;;; emacs-rc-erlang.el ends here

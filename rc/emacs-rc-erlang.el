;;; emacs-rc-erlang.el ---

(require 'em-glob)

(setq erlang-root-dir "/usr/lib/erlang")

(add-to-list 'ac-modes 'erlang-mode)

(defun directory-files-glob (path)
  (directory-files (file-name-directory path)
                   t
                   (eshell-glob-regexp (file-name-nondirectory path))))

(defun directory-any-file-glob (path)
  (car (directory-files-glob path)))

(when (string= system-type "gnu/linux")
  (add-to-list 'load-path (concat
                           (file-name-as-directory
                            (directory-any-file-glob
                             (concat erlang-root-dir "/lib/tools-*")))
                           "emacs")))

(when (and (require 'erlang-start nil t)
           (require 'erlang-flymake nil t))
  (erlang-flymake-only-on-save))


;;; emacs-rc-erlang.el ends here

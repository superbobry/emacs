;; Turn off mouse interface early in startup to avoid momentary display
;; (c) Emacs Starter Kit
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq
 root-dir (file-name-directory (or (buffer-file-name)
                                   load-file-name))
 custom-file (concat root-dir "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(defun load-init (modules)
  "Loads initialization files."
  (mapc (lambda (name)
          (load (format "emacs-rc-%s" name)))
        modules))

(defun autocompile ()
  "Compiles itself if this is config file."
  (interactive)
  (if (or
       (string-match ".emacs.d/rc/emacs-rc-[a-z]+.el$" (buffer-file-name))
       (string-match ".emacs.d/init.el$" (buffer-file-name)))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

(add-to-list 'load-path root-dir)
(add-to-list 'load-path (concat root-dir "packages"))
(add-to-list 'load-path (concat root-dir "packages/auto-complete"))
(add-to-list 'load-path (concat root-dir "packages/flymake"))
(add-to-list 'load-path (concat root-dir "packages/themes"))
(add-to-list 'load-path (concat root-dir "packages/yasnippet"))
(add-to-list 'load-path (concat root-dir "packages/rainbow-mode"))
(add-to-list 'load-path (concat root-dir "rc"))

(load-init '(auto-complete auto-insert auctex ccmode defuns erlang email
                           flymake haskell ido io js lisp lua local markup
                           org python yasnippet vcs

                           bindings))

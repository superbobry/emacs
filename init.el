;; Turn off mouse interface early in startup to avoid momentary display
;; (c) Emacs Starter Kit
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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
(add-to-list 'load-path (concat root-dir "rc"))
(add-to-list 'load-path (concat root-dir "el-get"))


(load-library "el-get/el-get")
(setq el-get-recipe-path (cons "~/code/el-get/recipes" el-get-recipe-path)
      el-get-sources
      '(auto-complete autopair auctex color-theme coffee-mode django-mode
                      el-get google-weather haskell-mode icomplete+ js2-mode
                      markdown-mode rainbow-mode python-mode session yasnippet
                      color-theme-mac-classic virtualenv

       (:name magit
              :after (lambda ()
                       (global-set-key (kbd "C-x C-g") 'magit-status)))

       (:name ahg
              :after (lambda ()
                       (global-set-key (kbd "C-x C-h") 'ahg-status)))

       (:name undo-tree
              :after (lambda ()
                       (global-set-key (kbd "C-x C-u") 'undo-tree-visualize)))

       (:name switch-window
              :after (lambda ()
                       (windmove-default-keybindings)
                       (setq windmove-wrap-around t)))

       (:name scratch
              :after (lambda ()
                       (global-set-key (kbd "C-x \\") 'scratch)))

       (:name nav
              :after (lambda ()
                       (global-set-key (kbd "C-x C-n") 'nav-toggle)))

       (:name po-mode
              :type http
              :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/gettext/gettext-tools/misc/po-mode.el?root=gettext")))

(el-get 'sync)
(el-get 'wait)

(load-init '(auto-complete auto-insert ccmode defuns flymake flyspell haskell
                           ido js lisp local markup org python yasnippet

                           bindings))

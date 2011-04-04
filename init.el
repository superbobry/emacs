;;; init.el ---


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(setq
 root-dir (file-name-directory (or (buffer-file-name)
                                   load-file-name)))


(add-to-list 'load-path root-dir)
(add-to-list 'load-path (concat root-dir "el-get"))


(load-library "el-get/el-get")
(setq el-get-sources
      '(el-get
        ;; generally useful stuff
        autopair auto-complete icomplete+ session scratch
        yasnippet grep+
        ;; vcs
        ahg magit
        ;; programming languages
        coffee-mode haskell-mode python-mode django-mode js2-mode
        tuareg-mode quack
        ;; markup
        auctex markdown-mode org-mode rainbow-mode
        ;; rest
        google-weather

        (:name nav
               :after (lambda ()
                        (setq nav-width 25)
                        (global-set-key (kbd "C-x C-n") 'nav-toggle)))))


(el-get 'sync)


(mapc (lambda (name)
        (load (concat root-dir
                      (format "rc/emacs-rc-%s" name)) t))
      '(auto-insert defuns erlang flymake flyspell haskell ido js lisp
                    local markup org python yasnippet

                    bindings))


(setq custom-file (concat root-dir "custom.el"))
(load custom-file t)


;;; init.el ends here

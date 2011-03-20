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
      '(el-get ahg auctex autopair auto-complete coffee-mode google-weather
               haskell-mode icomplete+ python-mode django-mode js2-mode
               magit markdown-mode org-mode rainbow-mode session
               scratch switch-window tuareg-mode quack undo-tree yasnippet

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

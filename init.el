;;; init.el ---


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(setq
 root-dir (file-name-directory (or (buffer-file-name)
                                   load-file-name)))


(add-to-list 'load-path root-dir)
(add-to-list 'load-path (concat root-dir "el-get/el-get"))


(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))


(setq el-get-sources
      '((:name js2-mode
               :type git
               :url "https://github.com/szimek/js2-mode.git"
               :compile "js2-mode.el"
               :post-init (lambda ()
                            (autoload 'js2-mode "js2-mode" nil t)))

        (:name nav
               :after (lambda ()
                        (setq nav-width 25)
                        (global-set-key (kbd "C-x C-n") 'nav)))))


(setq el-get-packages
      (append
       '(el-get
         ;; generally useful stuff
         autopair auto-complete icomplete+ scratch yasnippet grep+
         ;; vcs
         magit
         ;; programming languages
         coffee-mode haskell-mode python-mode tuareg-mode
         ;; markup
         auctex markdown-mode org-mode rainbow-mode
         ;; rest
         google-weather pkgbuild-mode deft)
       (mapcar 'el-get-source-name el-get-sources)))


(el-get 'sync el-get-packages)


(mapc (lambda (name)
        (load (concat root-dir
                      (format "rc/emacs-rc-%s" name))))
      '(auctex defuns erlang flymake flyspell haskell ido js lisp local
               markup org python

               bindings))


(setq custom-file (concat root-dir "custom.el"))
(load custom-file t)


;;; init.el ends here

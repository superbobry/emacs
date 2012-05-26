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
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s) (goto-char (point-max)) (eval-print-last-sexp))))

;; A quickfix for the missing `put-clojure-indent' function.
(unless (functionp 'put-clojure-indent)
  (defun put-clojure-indent (sym indent)))


(setq el-get-packages
      (append
       '(el-get
         ;; generally useful stuff
         autopair auto-complete icomplete+ yasnippet
         ;; vcs
         magit
         ;; programming languages
         coffee-mode haskell-mode clojure-mode python-mode
         ;; markup
         markdown-mode
         ;; rest
         pkgbuild-mode deft)
       (mapcar 'el-get-source-name el-get-sources)))


(el-get 'sync el-get-packages)


(mapc (lambda (name)
        (load (concat root-dir
                      (format "rc/emacs-rc-%s" name))))
      '(auctex defuns flymake flyspell haskell ido js lisp local markup python
               cc erlang math

               bindings))

(setq custom-file (concat root-dir "custom.el"))
(load custom-file t)


;;; init.el ends here

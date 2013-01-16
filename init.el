;;; init.el ---


(defvar bobry-dir (file-name-directory (or (buffer-file-name)
                                           load-file-name))
  "The root dir of the Emacs configuration.")

(defvar bobry-snippets-dir (concat bobry-dir "snippets/")
  "This folder stores custom yasnippet bundles.")

(defvar bobry-cache-dir (concat bobry-dir "cache/")
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p bobry-cache-dir)
  (make-directory bobry-cache-dir))


(add-to-list 'load-path bobry-dir)
(add-to-list 'load-path (concat bobry-dir "el-get/el-get"))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-sources
      '((:name golden-ratio
               :website "https://github.com/roman/golden-ratio.el"
               :description "Automatic resizing of Emacs windows to the golden ratio."
               :type github
               :pkgname "roman/golden-ratio.el"
               :features "golden-ratio"
               :after (progn (golden-ratio-enable)))
        (:name color-theme-solarized
               :description "Emacs highlighting using Ethan Schoonover's Solarized color scheme"
               :type github
               :pkgname "sellout/emacs-color-theme-solarized"
               :prepare (progn
                          (add-to-list 'custom-theme-load-path default-directory)
                          (load-theme 'solarized-dark)))
        (:name sml-mode
               :type elpa)))

(setq bobry-packages
      (append
       '(el-get
         ;; generally useful stuff
         auto-complete icomplete+ yasnippet volatile-highlights
         ;; vcs
         magit
         ;; programming languages
         coffee-mode haskell-mode clojure-mode python-mode ess
         ;; markup
         markdown-mode
         ;; rest
         powerline)
       (mapcar 'el-get-source-name el-get-sources)))


(el-get 'sync bobry-packages)

;; OS X specific settings
(when (eq system-type 'darwin)
  (load "rc/emacs-rc-osx"))

;; ... roll out the thing!
(load "rc/emacs-rc-ui")
(load "rc/emacs-rc-defuns")
(load "rc/emacs-rc-languages")
(load "rc/emacs-rc-markup")
(load "rc/emacs-rc-editor")
(load "rc/emacs-rc-flymake")
(load "rc/emacs-rc-flyspell")
(load "rc/emacs-rc-bindings")

(setq custom-file (concat bobry-dir "custom.el"))
(load custom-file t)


;;; init.el ends here

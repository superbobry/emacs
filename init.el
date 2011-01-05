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
(setq el-get-sources
      '(auto-complete ahg autopair auctex color-theme coffee-mode django-mode
                      el-get emacs-goodies-el google-weather haskell-mode
                      icomplete+ magit rainbow-mode python-mode scratch
                      switch-window undo-tree yasnippet
       (:name js2-mode
              :type git-svn
              :url "http://js2-mode.googlecode.com/svn/trunk/")
       (:name nav
              :type git-svn
              :url "http://emacs-nav.googlecode.com/svn/trunk/")
       (:name soy-mode
              :type git
              :url "https://github.com/toomore-such/soy-mode.git")
       (:name po-mode
              :type http
              :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/gettext/gettext-tools/misc/po-mode.el?root=gettext")
       ;; (:name color-theme-desert
       ;;        :type git
       ;;        :url "https://github.com/superbobry/color-theme-desert.git"
       ;;        :features color-theme-desert
       ;;        :after (lambda () (color-theme-desert)))
       (:name color-theme-mac-classic
              :type git
              :url "https://github.com/jbw/color-theme-mac-classic.git"
              :features color-theme-mac-classic
              :after (lambda () (color-theme-mac-classic)))))

(el-get)

(load-init '(auto-complete auto-insert ccmode defuns flymake flyspell haskell
                           ido js lisp local markup org python vcs yasnippet

                           bindings))

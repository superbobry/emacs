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


(load-library "/home/bobry/code/el-get/el-get")
(setq el-get-sources
      '(auto-complete ahg autopair color-theme color-theme-twilight django-mode
                      google-weather haskell-mode rainbow-mode scratch
                      session yasnippet

       ;; doesn't work that well :(
       ;; (:name bm
       ;;        :type http
       ;;        :url "http://ftp.twaren.net/Unix/NonGNU/bm/bm-1.50.el")
       (:name icomplete+
              :type emacswiki)
       (:name js2-mode
              :type git-svn
              :url "http://js2-mode.googlecode.com/svn/trunk/")
       (:name nav
              :type git-svn
              :url "http://emacs-nav.googlecode.com/svn/trunk/")
       (:name coffee-mode
              :type git
              :url "https://github.com/defunkt/coffee-mode.git")
       (:name python-mode
              :type bzr
              :url "https://code.launchpad.net/~python-mode-devs/python-mode/python-mode"
              :features python-mode)
       (:name soy-mode
              :type git
              :url "https://github.com/toomore-such/soy-mode.git")
       (:name markdown-mode
              :type git
              :url "https://github.com/defunkt/markdown-mode.git")
       (:name quack
              :type http
              :url "http://www.neilvandyke.org/quack/quack.el")
       (:name color-theme-subdued
              :type git
              :url "https://github.com/jbw/color-theme-subdued.git"
              :features color-theme-subdued)))

(el-get)

(load-init '(auto-complete auto-insert ccmode defuns erlang flymake
                           flyspell haskell ido js lisp local markup org
                           python vcs yasnippet

                           bindings))

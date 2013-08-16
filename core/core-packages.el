;;; core-packages.el ---

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-user-dir (expand-file-name "elpa" bobry-dir))
(package-initialize)

(defvar bobry-packages
  '(dash flycheck ido-ubiquitous auto-complete hi2 flx-ido
         magit rainbow-mode smex powerline
         scratch golden-ratio volatile-highlights solarized-theme
         coffee-mode haskell-mode clojure-mode python-mode ess ghc
         markdown-mode diminish undo-tree exec-path-from-shell
         yasnippet dropdown-list)
  "A list of packages to ensure are installed at launch.")

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      bobry-packages)


;;; core-packages.el ends here

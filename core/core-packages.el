;;; core-packages.el ---

(require 'cl)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-user-dir (local-file-name "elpa"))

(when (not package-archive-contents)
  (package-refresh-contents))

(package-initialize)

(defvar bobry-packages
  '(dash flycheck ido-ubiquitous auto-complete ac-math hi2 flx-ido
         magit rainbow-mode smex powerline auctex
         scratch golden-ratio volatile-highlights base16-theme
         coffee-mode haskell-mode ghc ghci-completion clojure-mode
         python-mode ess markdown-mode diminish undo-tree
         exec-path-from-shell yasnippet dropdown-list
         anzu smartparens ag projectile use-package)
  "A list of packages to ensure are installed at launch.")

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      bobry-packages)

(require 'use-package)


;;; core-packages.el ends here

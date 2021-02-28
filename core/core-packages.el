;;; core-packages.el ---

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq package-user-dir (local-file-name "elpa"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar bobry-packages
  '(diminish exec-path-from-shell use-package)
  "A list of packages to ensure are installed at launch.")

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      bobry-packages)

(eval-when-compile
  (require 'use-package))

(setq use-package-always-pin "melpa")

(require 'diminish)
(require 'bind-key)

;;; core-packages.el ends here

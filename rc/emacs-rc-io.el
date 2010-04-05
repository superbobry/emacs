;;; emacs-rc-io.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'io-mode)
;; I'm pretty happy with the defaults, really :)

(add-hook 'io-mode-hook 'run-coding-hook)

;;; emacs-rc-io.el ends here
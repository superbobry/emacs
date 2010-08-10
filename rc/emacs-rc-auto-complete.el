;;; emacs-rc-autocomplete.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet


(require 'auto-complete)

(add-to-list 'ac-dictionary-directories
             (concat root-dir "/packages/auto-complete/dict")

(require 'auto-complete-config)
(ac-config-default)

(setq
 ac-comphist-file (concat root-dir "/cache/ac-comphist.dat")
 ac-ignore-case nil)


;;; emacs-rc-autocomplete.el ends here

;;; emacs-rc-autocomplete.el ---


(require 'auto-complete)

(add-to-list 'ac-dictionary-directories
             (concat root-dir "/packages/auto-complete/dict"))

(require 'auto-complete-config)
(ac-config-default)

(setq
 ac-comphist-file (concat root-dir "/cache/ac-comphist.dat")
 ac-candidate-limit 20
 ac-ignore-case nil)


;;; emacs-rc-autocomplete.el ends here

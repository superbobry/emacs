;;; emacs-rc-ido.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet


(require 'ido)
(ido-mode t)
(ido-everywhere t)

(setq
 ido-confirm-unique-completion t
 ido-create-new-buffer 'always
 ido-enable-flex-matching t
 ido-enable-prefix nil
 ido-max-prospects 10
 ido-use-filename-at-point 'guess)


;;; emacs-rc-ido.el ends here
;;; emacs-rs-js.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'espresso)
(require 'coffee-mode) ;; nice couple :)

(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(add-hook 'espresso-mode-hook 'run-coding-hook)

(setq espresso-indent-level 2)

;; Note: doesn't work with autopair.el :/
;; (eval-after-load 'espresso
;;   '(progn ;; fixes problem with pretty function font-lock
;;           (define-key espresso-mode-map (kbd ",") 'self-insert-command)
;;           (font-lock-add-keywords
;;            'espresso-mode `(("\\(function *\\)("
;;                              (0 (progn (compose-region (match-beginning 1)
;;                                                        (match-end 1) "ƒ")
;;                                        nil)))))))

;;; emacs-rs-js.el ends here
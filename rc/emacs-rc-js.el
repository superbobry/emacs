;;; emacs-rs-js.el ---


(when (require 'js2-mode nil t)
  (add-hook 'js2-mode-hook 'run-coding-hook)
  (add-hook 'js2-mode-hook
            '(lambda ()
               (font-lock-add-keywords
                'js2-mode `(("\\(function *\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))))
               (autopair-mode)))

  (setq js2-basic-offset 2
        js2-auto-indent-p 'nil
        js2-bounce-indent-p t
        js2-global-externs '("console")
        js2-electric-keys '(";" "," "*", "{", "(")))


(when (require 'coffee-mode nil t)
  (add-hook 'coffee-mode-hook 'run-coding-hook)
  (add-hook 'coffee-mode-hook
            '(lambda ()
               (set (make-local-variable 'tab-width) 2)
               (setq coffee-args-compile '("-c", "--bare")
                     coffee-debug-mode t)

               ;; Compile '.coffee' files on every save
               (and (file-exists-p (buffer-file-name))
                    (file-exists-p (coffee-compiled-file-name))
                    (coffee-cos-mode t)))))


;;; emacs-rs-js.el ends here

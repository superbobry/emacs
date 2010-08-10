;;; emacs-rc-yasnippet.el ---


(require 'yasnippet)

(setq
 yas/prompt-functions '(yas/dropdown-prompt
                        yas/completing-prompt))

(eval-after-load "yasnippet"
  '(progn
     (yas/initialize)
     (yas/load-directory (concat (getenv "HOME") "/.emacs.d/snippets"))))

;; (add-hook 'after-save-hook
;; 	  (if (string-match my-yasnippet-dir (or buffer-file-name ""))
;; 	      (yas/load-directory my-yasnippet-dir)))


;;; emacs-rc-yasnippet.el ends here

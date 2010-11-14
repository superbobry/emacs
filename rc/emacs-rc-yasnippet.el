;;; emacs-rc-yasnippet.el ---


(require 'yasnippet)
(require 'dropdown-list)

(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/x-prompt
                             yas/ido-prompt)
      yas/snippet-dirs (list (concat root-dir "snippets")
                             (concat el-get-dir "yasnippet/snippets")))

(yas/initialize)

;; (add-hook 'after-save-hook
;; 	  (if (string-match my-yasnippet-dir (or buffer-file-name ""))
;; 	      (yas/load-directory my-yasnippet-dir)))


;;; emacs-rc-yasnippet.el ends here


;;; rc-flyspell.el ---


(require 'flyspell)
(require 'ispell)

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra") ;; making aspell work faster
      ispell-have-new-look t
      ispell-highlight-face 'flyspell-incorrect
      ispell-dictionary "english"

      flyspell-delay 1
      flyspell-issue-message-flag nil         ;; no messages, while checking
      flyspell-default-dictionary "english")

(add-hook 'org-mode-hook 'flyspell-mode)


;;; rc-flyspell.el ends here

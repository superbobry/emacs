;;; emacs-rc-jabber.el ---


(require 'jabber)
(require 'jabber-bookmarks)
(require 'jabber-history)

(setq jabber-account-list
      '(("superbobry@jabber.ru"
         (:network-server . "jabber.ru")
         (:connection-type . starttls))
;        ("superbobry@gmail.com"
;         (:network-server . "talk.google.com")
;         (:connection-type . ssl)
;         (:port: . 5223))
        ))


(defun my-jc-mode-hook ()
;  (flyspell-mode)
;  (autosmiley-mode)
  (setq fill-column 120)

  (progn
    (buffer-face-mode t)
    (make-face 'buffer-face)
    (set-face-attribute 'buffer-face nil :family "Liberation Mono" :height 110)
    (buffer-face-set 'buffer-face)
    ))


(add-hook 'jabber-chat-mode-hook 'my-jc-mode-hook)
(add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

(jabber-roster-toggle-binding-display)
(jabber-roster-toggle-offline-display)

(setq
 jabber-auto-reconnect t
 jabber-roster-line-format " %c %-25n %u %-8s"
 jabber-groupchat-buffer-format "*-jg-%n-*"
 jabber-chat-buffer-format "*-jc-%n-*"
 jabber-muc-private-buffer-format "*-jmuc-priv-%g-%n-*"
 jabber-vcard-avatars-retrieve nil
 jabber-history-enabled nil
 jabber-use-global-history nil
 jabber-resource "home"
 jabber-muc-completion-delimiter ", "
 jabber-chat-local-prompt-format "[%t] %n: "
 jabber-chat-foreign-prompt-format jabber-chat-local-prompt-format
 jabber-chat-system-prompt-format "[%t] ")


;;; emacs-rc-jabber.el ends here


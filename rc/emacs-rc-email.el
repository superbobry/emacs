;;; emacs-rc-email.el ---


(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "superbobry@gmail.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-use-modified-utf7 t
      elmo-enable-disconnected-operation t)

;; SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "superbobry"
      wl-smtp-posting-server "smtp.gmail.com"
      wl-local-domain "gmail.com")

;; Folders
(setq wl-folders-file (concat root-dir ".folders")
      wl-fcc "%[Gmail]/Sent Mail"
      wl-fcc-force-as-read t            ;; mark sent messages as read
      wl-default-folder "Inbox"
      wl-default-spec "%"
      wl-draft-folder "%[Gmail]/Drafts" ;; Gmail IMAP
      wl-trash-folder "%[Gmail]/Trash")

;; Display
(setq wl-stay-folder-window t           ;; show the folder pane (left)
      wl-folder-window-width 23         ;; toggle on/off with 'i'
      wl-auto-select-first t            ;; display the first message in the folder
      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^Message-Id:"
        "^\\(Posted\\|Date\\):"
        )
      wl-summary-width	nil
      wl-summary-line-format "%T%P %30(%t%[%c %f %]%) %55s %D/%M-%h:%m"
      wl-message-window-size '(1 . 3)
      wl-message-sort-field-list '("^Date" "^From" "^Subject" "^To" "^Cc"))

;; Threads
(setq wl-thread-indent-level 2
      wl-thread-have-younger-brother-str "+"
      wl-thread-youngest-child-str "+"
      wl-thread-vertical-str "|"
      wl-thread-horizontal-str "-"
      wl-thread-space-str " ")

;; Misc
(setq wl-from "Sergei Lebedev <superbobry@gmail.com>"
      wl-forward-subject-prefix "Fwd: "  ;; use "Fwd: " not "Forward: "
      wl-draft-reply-without-argument-list
      '(("Reply-To" ("Reply-To") nil nil)
        ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
        ("From" ("From") nil nil)))


;; Check outgoing mail (suggested by Masaru Nomiya on the WL mailing list)
(defun wl-draft-subject-check ()
  "Check whether the message has a subject before sending."
  (if (and (< (length (std11-field-body "Subject")) 1)
        (null (y-or-n-p "No subject! Send current draft?")))
      (error "Abort.")))

(defun wl-draft-attachment-check ()
  "If attachment is mention but none included, warn the the user."
  (save-excursion
    (goto-char 0)
    (unless ;; don't we have an attachment?

      (re-search-forward "^Content-Disposition: attachment" nil t)
     (when ;; no attachment; did we mention an attachment?
        (re-search-forward "attach" nil t)
        (unless (y-or-n-p "Possibly missing an attachment. Send current draft?")
          (error "Abort."))))))

(add-hook 'wl-mail-send-pre-hook 'wl-draft-subject-check)
(add-hook 'wl-mail-send-pre-hook 'wl-draft-attachment-check)

;; This is supposed to fix UTF-8 issues (and it does!) ...
(setq-default mime-transfer-level 8)


;;; emacs-rc-email.el ends here

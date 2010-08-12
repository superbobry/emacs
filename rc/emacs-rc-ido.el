;;; emacs-rc-ido.el ---


(require 'ido)
(ido-mode 'both)
(ido-everywhere t)

(setq
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*SPEED" "^\*Mess" "^\*Back" ".*Completion"
   "^\*Ido" "^\*trace" "^\*compilation" "^Folder\\|^Summary\\|^Message\*")
 ido-case-fold  t   ;; be case-insensitive
 ido-confirm-unique-completion t     ;; wait for RET, even with unique completion
 ido-create-new-buffer 'always
 ido-enable-flex-matching nil        ;; not, too smart, baby ...
 ido-enable-last-directory-history t ;; remember last used dirs
 ido-max-work-directory-list 30      ;; should be enough
 ido-max-work-file-list 50           ;; remember many
 ido-use-filename-at-point nil       ;; don't use filename at point
 ido-use-url-at-point nil            ;; don't use url at point
 ido-enable-prefix nil
 ido-max-prospects 10)

(setq confirm-nonexistent-file-or-buffer nil)


;;; emacs-rc-ido.el ends here

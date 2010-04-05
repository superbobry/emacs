;;; flymake-erlang.el

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'flymake-common)

(defun flymake-erlang-init ()
    (flymake-template-init
     (concat (getenv "HOME") "/emacs.d/packages/erlang/eflymake")))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

(provide 'flymake-erlang)

;;; flymake-erlang.el ends here
;;; flymake-python.el

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'flymake-common)

(defun flymake-python-init (&optional checker)
  (or checker (setq checker "pyflakes"))
  (flymake-template-init checker))

(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-init))

(provide 'flymake-python)

;;; flymake-python.el ends here

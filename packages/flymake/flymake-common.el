;;; flymake-common.el

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'flymake)

(defun flymake-template-init (checker)
    (let* ((temp-file
	    (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
           (local-file
	    (file-relative-name temp-file (file-name-directory buffer-file-name))))
      (list checker (list local-file))))

(provide 'flymake-common)

;;; flymake-common.el ends here
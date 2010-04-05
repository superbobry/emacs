;;; emacs-rc-auctex.el ---

;; Copyright (C) Sergei Lebedev
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'tex-site)
(require 'preview-latex)

(setq
 TeX-auto-save t
 TeX-parse-self t
 TeX-DVI-via-PDFTeX t)


(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (TeX-fold-mode 1)
             (TeX-PDF-mode 1)
             (outline-minor-mode 1)))

;;; emacs-rc-auctex.el ends her
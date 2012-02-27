;;; emacs-rc-ocaml.el ---


(when (require 'typerex nil t)
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
  (autoload 'typerex-mode "typerex" "Major mode for editing OCaml code" t)
  (setq ocp-server-command "/usr/local/bin/ocp-wizard"
        typerex-in-indent 0
        ocp-syntax-coloring "tuareg_like"
        ocp-auto-complete nil))


;;; emacs-rc-ocaml.el ends here

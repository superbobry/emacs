;;; emacs-rc-maxima.el ---


(add-to-list 'load-path "/usr/share/maxima/5.21.1/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)


;;; emacs-rc-maxima.el ends here

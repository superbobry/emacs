;;; emacs-rc-python.el ---


(when (require 'python-mode nil t)
  (setq py-python-command "python2")

  (add-hook 'python-mode-hook 'run-coding-hook)
  (add-hook 'python-mode-hook
            #'(lambda ()
                (setq autopair-handle-action-fns
                      (list #'autopair-default-handle-action
                            #'autopair-python-triple-quote-action)))))


;;; emacs-rc-python.el ends here

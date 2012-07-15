;;; emacs-rc-flymake.el ---

(require 'flymake)

(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (nth 1 elem))))
        (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
    ((null (flymake-ler-file err))
     ;; normal message do your thing
     (flymake-ler-text err))
    (t ;; could not compile err
     (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook)))

(defun flymake-template-init (checker)
  "Create init function, running a given checker program."
  (let* ((temp-file
          (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
         (local-file
          (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list checker (list local-file))))

;; Python

(defun flymake-python-init (&optional checker)
  (flymake-template-init (or checker "pyflakes")))

(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-init))


;;; emacs-rc-flymake.el ends here

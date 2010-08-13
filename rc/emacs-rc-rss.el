;;; emacs-rc-rss.el ---


(require 'newsticker)

(newsticker-start)

(defadvice newsticker-show-news (around
                                 elscreen-newsticker-show-news
                                 activate)
  "Advising `newsticker-show-news' to be launched in a separate screen."
  (elscreen-create-buffer ad-do-it))


;;; emacs-rc-rss.el ends here

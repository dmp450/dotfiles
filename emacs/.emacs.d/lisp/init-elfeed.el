(use-package elfeed
  :ensure t
  :config
  ;; Star and unstar articles
  ;; http://pragmaticemacs.com/emacs/star-and-unstar-articles-in-elfeed/
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))

  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))

  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist))

(use-package elfeed-org
  :ensure t
  :custom
  (rmh-elfeed-org-files (list "~/.emacs.d/feeds.org"))
  :config
  (elfeed-org))

(provide 'init-elfeed)

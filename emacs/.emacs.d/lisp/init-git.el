(use-package magit
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'xah-fly-insert-mode-activate))

(provide 'init-git)

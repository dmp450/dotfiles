(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "dvorak")
  (defun xah-fly-keys-enable-insert-mode-unless-prog-mode ()
    (if (derived-mode-p 'prog-mode)
	(xah-fly-command-mode-activate)
    (xah-fly-insert-mode-activate)))
  (add-hook 'change-major-mode-after-body-hook #'xah-fly-keys-enable-insert-mode-unless-prog-mode)
  (xah-fly-keys 1))

(provide 'init-xah-fly-keys)

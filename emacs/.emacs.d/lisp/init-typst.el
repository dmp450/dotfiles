(use-package websocket)

(use-package typst-preview
  :straight (typst-preview :type git :host github :repo "havarddj/typst-preview.el")
  :config
  (setq typst-preview-browser "default")
  (define-key typst-preview-mode-map (kbd "C-c C-j") 'typst-preview-send-position)
  )

(use-package typst-ts-mode
  :ensure t
  :straight (typst-ts-mode :type git :host codeberg :repo "meow_king/typst-ts-mode")
  :after lsp-mode
  :mode ("\\.typ\\'" . typst-ts-mode)
  :hook (typst-ts-mode . lsp-deferred)
  :custom (typst-ts-mode-watch-options "--open")
  :config
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
  (lsp-register-client
   (make-lsp-client
         :new-connection (lsp-stdio-connection "tinymist")
         :major-modes '(typst-ts-mode)
         :server-id 'typst-lsp))
  )


(provide 'init-typst)

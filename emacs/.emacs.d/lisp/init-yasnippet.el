(use-package yasnippet
  :ensure t
  ;; :hook ((LaTeX-mode . yas-minor-mode))
  :config
  (setq yas-new-snippet-default
	 "# -*- mode: snippet -*-
# contributor: Derek Perrin <derek@derekperrin.com>
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
$0`(yas-escape-text yas-selected-text)`")
  (setq yas-triggers-in-field t)
  (yas-global-mode t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all))

(provide 'init-yasnippet)

;; Enable which-key https://github.com/justbur/emacs-which-key
(use-package which-key
  :init (which-key-mode 1))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
		     :includes (vertico-indexed vertico-flat
				vertico-grid vertico-mouse
				vertico-buffer vertico-quick
				vertico-repeat vertico-reverse
				vertico-directory
				vertico-multiform
				vertico-unobstrusive ))
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode)
  (vertico-multiform-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex)))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(provide 'init-navigation)

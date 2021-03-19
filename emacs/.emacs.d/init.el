;; Set emacs default stuff
(setq inhibit-startup-message t)	; Disable the startup tutorial screen
(setq sentence-end-double-space nil)	; Single space should end a sentence.
(setq-default fill-column 80)
(setq column-number-mode t)		; Enable column number mode 
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq initial-scratch-message "Welcome to Emacs :-)\n")
(setq ring-bell-function 'ignore)
(setq delete-old-versions -1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))) ; set a backups directory
(defalias 'yes-or-no-p 'y-or-n-p)	; change yes/no prompts to y/n
; (global-display-line-numbers-mode)


;; Start Emacs maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; disable a bunch of tool bar and stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

;; Do not use 'init.el' for 'custom-*' code. Use 'custom.el'
(setq custom-file "~/.emacs.d/custom-file.el")

;; We need to load the custom file because emacs may not have executed
;; the code in it before getting to this point.
(load-file custom-file)

;; Bootstrap code for straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package and make use-package install with straight-use-package
;; Effectively sets ':straight t' on everything.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)





;; Additional packages and their configurations

;; Enable general.el. https://github.com/noctuid/general.el
(use-package general
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "TAB" '(other-window :which-key "prev buffer")))
 

(use-package spacemacs-theme
  :no-require t
  :config
  ;; Don't use a different background color for comments  
  (setq spacemacs-theme-comment-bg nil)
  ;; Italicize comments
  
  (setq spacemacs-theme-comment-italic t)
  (load-theme 'spacemacs-dark))

(use-package evil
  :init (evil-mode 1)
  :config
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil))

;; Enable which-key https://github.com/justbur/emacs-which-key
(use-package which-key
  :init (which-key-mode 1))

;; Enable ivy, counsel, and swiper https://github.com/abo-abo/swiper
(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-height 20)
  (setq ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-fuzzy) ; enable fuzzy searching
	  (t . ivy--regex-plus))))

(use-package counsel
  :init (counsel-mode 1)
  :general
  (:prefix "C-c"
	   "f" '(:ignore t :which-key "files") 
	   "b" 'ivy-switch-buffer ; Change buffer using ivy
	   "ff" 'counsel-find-file
	   "fr" 'counsel-recentf))


(use-package company
  :init
  (global-company-mode t)
  :general
  (company-active-map
  "C-n" 'company-select-next
  "C-p" 'company-select-previous))
 
;; Enable the AUCTeX package https://www.gnu.org/software/auctex/manual/auctex.index.html
;; See also documentation for preview-latex
;; https://www.gnu.org/software/auctex/manual/preview-latex.index.html
(use-package tex
  :straight auctex
  :config
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (setq preview-auto-reveal t))

;; Enable the cdlatex minor mode https://staff.fnwi.uva.nl/c.dominik/Tools/cdlatex/
(use-package cdlatex
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)) ; turn on CDLaTeX with AUCTex LaTeX mode.

(use-package auctex-latexmk
  :init (auctex-latexmk-setup)
  :config
  (setq-default TeX-master nil))

(use-package latex-preview-pane
  :init (latex-preview-pane-enable)
  :config
  (setq latex-preview-pane-multifile-mode 'auctex))

;; Enable company-math for autocompletion with TeX symbols
;; https://github.com/vspinu/company-math If on Ubuntu, install
;; ttf-ancient-fonts to fix font problems. See
;; https://github.com/bixuanzju/emacs.d/blob/master/emacs-init.org for an
;; example on how to configure company-math and other stuff.
(use-package company-math
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (defun my-latex-mode-setup ()
    (setq-local company-backends
		(append '(company-math-symbols-latex company-latex-commands)
			company-backends)))
    (add-hook 'TeX-mode-hook 'my-latex-mode-setup))

(use-package go-mode
  :mode "\\*\\.go")

;; Set relative line numbers https://github.com/xcodebuild/nlinum-relative
(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-redisplay-delay 0)
  (global-nlinum-relative-mode))

;; add support for markdown
 (use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")) 

(use-package ein
  )

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
(setq tramp-default-method "ssh")
(setq vc-follow-symlinks nil)
(defalias 'yes-or-no-p 'y-or-n-p)	; change yes/no prompts to y/n
(add-to-list 'exec-path "/home/dperrin/.bin")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
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

;; Add some sane parentheses/brackets defaults globally.
(electric-pair-mode 1)
(show-paren-mode 1)

;; Enable global line number mode as per
;; http://ergoemacs.org/emacs/emacs_line_number_mode.html
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Do not use 'init.el' for 'custom-*' code. Use 'custom.el'
(setq custom-file "~/.emacs.d/custom-file.el")

;; We need to load the custom file because emacs may not have executed
;; the code in it before getting to this point.
(load-file custom-file)

;; Bootstrap code for straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
       user-emacs-directory))
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



;; Experimental! Use Xah Fly Keys instead of evil mode.
;; Disabled general.el, evil, and nlinum-relative.
(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "dvorak")
  (xah-fly-keys 1))

;; Additional packages and their configurations

;; Enable general.el. https://github.com/noctuid/general.el
;; (use-package general
;;   :config
;;   (general-define-key
;;    :states '(normal visual insert emacs)
;;    :prefix "SPC"
;;    :non-normal-prefix "C-SPC"
;;    "TAB" '(other-window :which-key "prev buffer")))
 
(use-package spacemacs-theme
  :no-require t
  :custom
  ;; Don't use a different background color for comments  
  (spacemacs-theme-comment-bg nil)
  ;; Italicize comments
  (spacemacs-theme-comment-italic t)
  :config
  (load-theme 'spacemacs-dark))

(use-package org
  :demand t:)

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
  (marginalia-align 'center)
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(basic orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

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

(use-package yasnippet
  :ensure t
  :hook (LaTeX-mode . yas-minor-mode)
  :config
  (setq yas-new-snippet-default
	 "# -*- mode: snippet -*-
# contributor: Derek Perrin <derek@derekperrin.com>
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
$0`(yas-escape-text yas-selected-text)`")
  (setq yas-triggers-in-field t))

;; Enable the AUCTeX package
;; https://www.gnu.org/software/auctex/manual/auctex.index.html See also
;; documentation for preview-latex
;; https://www.gnu.org/software/auctex/manual/preview-latex.index.html
(use-package tex-site
  :straight auctex
  :hook ((LaTeX-mode . prettify-symbols-mode)
	 (LaTeX-mode . reftex-mode)
	 (LaTeX-mode . outline-minor-mode)
	 (LaTeX-mode . flyspell-mode))
  :custom
  (TeX-master nil)
  (TeX-auto-save t)
  (preview-auto-reveal t)
  (reftex-plug-into-AUCTeX t)
  :config
  (add-hook 'LaTeX-mode-hook
	    (defun preview-larger-previews ()
	      (setq preview-scale-function
		    (lambda () (* 1.5
				  (funcall (preview-scale-from-face))))))))

;; Enable the cdlatex minor mode https://staff.fnwi.uva.nl/c.dominik/Tools/cdlatex/
(use-package cdlatex
  :after tex
  :custom
  (cdlatex-paired-parens "$[({")
  ;; Create some new environments
  (cdlatex-env-alist
	'(("axiom" "\\begin{axm}\n?\n\\end{axm}" nil)
	  ("theorem" "\\begin{thm}\n?\n\\end{thm}" nil)
	  ("definition" "\\begin{defn}\n?\n\\end{defn}" nil)
	  ("remark" "\\begin{rmk}\n?\n\\end{rmk}" nil)
	  ("corollary" "\\begin{cor}\n?\n\\end{cor}" nil)
	  ("proposition" "\\begin{prop}\n?\n\\end{prop}" nil)))
  ;; set some nice shortcuts for various environments
  (cdlatex-command-alist
	'(("axm" "Insert axiom env" "" cdlatex-environment ("axiom") t nil)
	  ("thm" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
	  ("defn" "Insert definition env" ""cdlatex-environment ("definition") t nil)
	  ("rmk" "Insert remark environment" ""cdlatex-environment ("remark") t nil)
	  ("cor" "Insert corollary environment" ""cdlatex-environment ("corollary") t nil)
	  ("prop" "Insert proposition environment" ""cdlatex-environment ("proposition") t nil)))
  :config
  ;; (define-key cdlatex-mode-map "(" nil)
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)) ; turn on CDLaTeX with AUCTex LaTeX mode.

(use-package latex-preview-pane
  :init (latex-preview-pane-enable)
  :custom
  (latex-preview-pane-multifile-mode 'auctex))

;; Enable company-math for autocompletion with TeX symbols
;; https://github.com/vspinu/company-math If on Ubuntu, install
;; ttf-ancient-fonts to fix font problems. See
;; https://github.com/bixuanzju/emacs.d/blob/master/emacs-init.org for an
;; example on how to configure company-math and other stuff.
(use-package company-math
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (setq-local company-backends (delete 'company-dabbrev company-backends))
  (defun my-latex-mode-setup ()
    (setq-local company-backends
		(append '(company-math-symbols-latex company-latex-commands)
			company-backends)))
    (add-hook 'TeX-mode-hook 'my-latex-mode-setup))

(use-package go-mode
  :mode "\\*\\.go")

;; Set relative line numbers https://github.com/xcodebuild/nlinum-relative
;; (use-package nlinum-relative
;;   :config
;;   (nlinum-relative-setup-evil)
;;   (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;;   (setq nlinum-relative-redisplay-delay 0)
;;   (global-nlinum-relative-mode))

;; add support for markdown
 (use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")) 

;; This is to use Jupyter notebooks within Emacs. For more info, see
;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein)

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'xah-fly-insert-mode-activate))

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


;; My mail configuration
;; TODO: Look at adding in folding perhaps?
(use-package mu4e
  :ensure t
  :config
  ;; Use mu4e for email in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  ;; mail fetching options
  (setq mu4e-get-mail-command "mbsync -a"
	mu4e-update-interval 300
	mu4e-index-cleanup nil
	mu4e-index-lazy-check t
	mu4e-change-filenames-when-moving t)

  ;; Set the 'd' key to move to trash instead of marking as trash.
  ;; This prevents fastmail from destroying the message.
  (fset 'my-move-to-trash "mt")
  (define-key mu4e-headers-mode-map (kbd "k") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "k") 'my-move-to-trash)

  ;; sending mail configuration
  (setq sendmail-program "/usr/bin/msmtp"
	send-mail-function 'smtpmail-send-it
	message-send-mail-function 'message-send-mail-with-sendmail
	message-citation-line-format "\nOn %Y-%m-%d at %R %Z, %f wrote:\n"
	message-citation-line-function 'message-insert-formatted-citation-line)

  (setq mu4e-contexts
	`(,(make-mu4e-context
	    :name "Personal"
	    :match-func (lambda (msg)
			  (when msg
			    (string-prefix-p "/fastmail" (mu4e-message-field msg :maildir))))
	    :vars '(
		    (user-full-name . "Derek Perrin")
		    (user-mail-address . "derek@derekperrin.com")
		    (mu4e-sent-folder . "/fastmail/Sent Items")
		    (mu4e-drafts-folder . "/fastmail/Drafts")
		    (mu4e-trash-folder . "/fastmail/Trash")
		    (mu4e-refile-folder . "/fastmail/Archive")
		    (mu4e-sent-messages-behavior . sent)
		    (mu4e-compose-signature-auto-include nil)))
	  ,(make-mu4e-context
	    :name "School"
	    :match-func (lambda (msg)
			  (when msg
			    (string-prefix-p "/uclive" (mu4e-message-field msg :maildir))))
	    :vars '(
		    (user-full-name . "Derek Perrin")
		    (user-mail-address . "derek.perrin@pg.canterbury.ac.nz")
		    (mu4e-sent-folder . "/uclive/Sent Items")
		    (mu4e-drafts-folder . "/uclive/Drafts")
		    (mu4e-trash-folder . "/uclive/Deleted Items")
		    (mu4e-refile-folder . "/uclive/Archive")
		    (mu4e-sent-messages-behavior . sent)
		    (mu4e-compose-signature-auto-include nil)))
	  ))
  (setq mu4e-context-policy 'pick-first)
  
  ;; Some maildir shortcuts
  (setq mu4e-maildir-shortcuts
	'( (:maildir "/fastmail/INBOX"		:key ?i)
	   (:maildir "/uclive/INBOX"            :key ?I)
	   (:maildir "/uclive/Archive"           :key ?A)
	   (:maildir "/fastmail/Drafts"		:key ?d)
	   (:maildir "/fastmail/Sent Items"	:key ?s)
	   (:maildir "/fastmail/Archive"         :key ?a)
	   (:maildir "/fastmail/Junk Mail"	:key ?j)
	   (:maildir "/fastmail/Trash"		:key ?t)))

  ;; display customization
  ;; custom thread headers found at https://mu-discuss.narkive.com/0A8jgd4g/fyi-nicer-threading-characters
  (setq mu4e-headers-fields
	'((:date		. 20)
	  (:flags		. 10)
	  (:from		. 30)
	  (:thread-subject	. nil))
	mu4e-headers-date-format "%Y.%m.%d %R"
	mu4e-headers-thread-child-prefix '("├>" . "├─➤ ")
	mu4e-headers-thread-last-child-prefix '("└>" . "└─➤ ")
	mu4e-headers-thread-orphan-prefix '("┬>" . "┬─➤ ")
	mu4e-headers-thread-single-orphan-prefix '("─>" . "──➤ ")
	;; The following two should have the same width.
	mu4e-headers-thread-connection-prefix '("│" . "│ ")
	mu4e-headers-thread-blank-prefix '(" " . " ")
	mu4e-headers-unread-mark '("u" . "🖂")
	mu4e-use-fancy-chars t)
  (setq message-kill-buffer-on-exit t)
  ;; disable command mode when starting mu4e
  (add-hook 'mu4e-main-mode-hook 'xah-fly-insert-mode-activate))

(use-package sage-shell-mode
  :ensure t
  :config
  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode))

;; Configure rcirc
(setq rcirc-server-alist
      '(("irc.libera.chat" :port 6697 :encryption tls
	 :channels ("##math" "##crypto")
	 :nick "materialranger")))
(add-hook 'rcirc-omit-mode
	  (lambda ()
	    ;; rcirc-omit-mode *always* toggles, so we disable it first
	    ;; so it can toggle to being enabled.
	    (setq rcirc-omit-mode nil)
	    (rcirc-omit-mode)))

;; add support for power mode! https://github.com/elizagamedev/power-mode.el
;; (use-package power-mode
;;     :straight (power-mode :type git :host github :repo "elizagamedev/power-mode.el")
;;     :init
;;     (add-hook 'after-init-hook #'power-mode))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package web-mode
  :mode
  (
   ".twig$"
   ".html?$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   )
  :custom
   (web-mode-markup-indent-offset 2)
   (web-mode-css-indent-offset 2)
   (web-mode-code-indent-offset 2)
   (web-mode-style-padding 2)
   (web-mode-script-padding 2)
   (web-mode-enable-auto-closing t)
   (web-mode-enable-auto-opening t)
   (web-mode-enable-auto-pairing t)
   (web-mode-enable-auto-indentation t)
   :config
  ;; Let smartparens handle auto closing brackets, e.g. {{ }} or {% %}
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/web/%2Bhtml.el#L56
  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
            (cl-loop for pair in (cdr alist)
                     unless (string-match-p "^[a-z-]" (cdr pair))
                     collect (cons (car pair)
                                   (string-trim-right (cdr pair)
                                                      "\\(?:>\\|]\\|}\\)+\\'"))))))
(use-package telega
  :load-path "~/.emacs.d/straight/repos/telega.el/"
  :custom
  (telega-sticker-animated-play "/home/dperrin/.bin/tgs2png")
  :ensure t)

(use-package w3m
  :ensure t)

(use-package rust-mode)

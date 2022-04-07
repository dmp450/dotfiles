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
  :config
  ;; Don't use a different background color for comments  
  (setq spacemacs-theme-comment-bg nil)
  ;; Italicize comments
  
  (setq spacemacs-theme-comment-italic t)
  (load-theme 'spacemacs-dark))

(use-package org
  :demand t:)

;; Enable which-key https://github.com/justbur/emacs-which-key
(use-package which-key
  :init (which-key-mode 1))

;; Enable ivy, counsel, and swiper https://github.com/abo-abo/swiper
(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-height 20)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-fuzzy) ; enable fuzzy searching
	  (t . ivy--regex-plus))))

(use-package counsel
  :init (counsel-mode 1))
;;   :general
;;   (:prefix "C-c"
;; 	   "f" '(:ignore t :which-key "files")
;; 	   "b" 'ivy-switch-buffer ; Change buffer using ivy
;; 	   "ff" 'counsel-find-file
;; 	   "fr" 'counsel-recentf))

(use-package company
  :init
  (global-company-mode t))
  ;; :general
  ;; (company-active-map
  ;; "C-n" 'company-select-next
  ;; "C-p" 'company-select-previous))
 
;; Enable the AUCTeX package
;; https://www.gnu.org/software/auctex/manual/auctex.index.html See also
;; documentation for preview-latex
;; https://www.gnu.org/software/auctex/manual/preview-latex.index.html
(use-package tex-site
  :straight auctex
  :config
  (setq TeX-master nil
	TeX-auto-save t
	preview-auto-reveal t
	reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell))

;; Enable the cdlatex minor mode https://staff.fnwi.uva.nl/c.dominik/Tools/cdlatex/
(use-package cdlatex
  :after tex
  :config
  ;; (define-key cdlatex-mode-map "(" nil)
  (setq cdlatex-paired-parens "$[({")
  ;; Create some new environments
  (setq cdlatex-env-alist
	'(("axiom" "\\begin{axm}\n?\n\\end{axm}" nil)
	  ("theorem" "\\begin{thm}\n?\n\\end{thm}" nil)
	  ("definition" "\\begin{defn}\n?\n\\end{defn}" nil)
	  ("remark" "\\begin{rmk}\n?\n\\end{rmk}" nil)
	  ("corollary" "\\begin{cor}\n?\n\\end{cor}" nil)
	  ("proposition" "\\begin{prop}\n?\n\\end{prop}" nil)))
  ;; set some nice shortcuts for various environments
  (setq cdlatex-command-alist
	'(("axm" "Insert axiom env" "" cdlatex-environment ("axiom") t nil)
	  ("thm" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
	  ("defn" "Insert definition env" ""cdlatex-environment ("definition") t nil)
	  ("rmk" "Insert remark environment" ""cdlatex-environment ("remark") t nil)
	  ("cor" "Insert corollary environment" ""cdlatex-environment ("corollary") t nil)
	  ("prop" "Insert proposition environment" ""cdlatex-environment ("proposition") t nil)))
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)) ; turn on CDLaTeX with AUCTex LaTeX mode.

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
(use-package ein
  )

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
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/feeds.org")))


;; My mail configuration
;; TODO: Look at adding in folding perhaps?
(use-package mu4e
  :ensure t
  :config
  ;; Use mu4e for email in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  ;; This stuff is the bare minimum needed for mu4e
  (setq mu4e-sent-folder "/Sent Items"
	mu4e-drafts-folder "/Drafts"
	mu4e-trash-folder "/Trash"
	mu4e-refile-folder "/Archive")

  ;; mail fetching options
  (setq mu4e-get-mail-command "mbsync -a"
	mu4e-update-interval 300
	mu4e-index-cleanup nil
	mu4e-index-lazy-check t
	mu4e-change-filenames-when-moving t)

  ;; Set the 'd' key to move to trash instead of marking as trash.
  ;; This prevents fastmail from destroying the message.
  (fset 'my-move-to-trash "mt")
  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

  ;; sending mail configuration
  (setq sendmail-program "/usr/bin/msmtp"
	send-mail-function 'smtpmail-send-it
	message-send-mail-function 'message-send-mail-with-sendmail
	message-citation-line-format "\nOn %Y-%m-%d at %R %Z, %f wrote:\n"
	message-citation-line-function 'message-insert-formatted-citation-line)

  ;; My sender information
  (setq user-mail-address "derek@derekperrin.com"
	user-full-name "Derek Perrin"
	mu4e-compose-signature-auto-include nil)

  ;; Some maildir shortcuts
  (setq mu4e-maildir-shortcuts
	'( (:maildir "/INBOX"		:key ?i)
	   (:maildir "/Drafts"		:key ?d)
	   (:maildir "/Sent Items"	:key ?s)
	   (:maildir "/Archive"         :key ?a)
	   (:maildir "/Shopping.Amazon" :key ?A)
	   (:maildir "/Shopping.kijiji" :key ?k)
	   (:maildir "/Junk Mail"	:key ?j)
	   (:maildir "/Trash"		:key ?t)))

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
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t)

  ;; Let smartparens handle auto closing brackets, e.g. {{ }} or {% %}
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/web/%2Bhtml.el#L56
  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
            (cl-loop for pair in (cdr alist)
                     unless (string-match-p "^[a-z-]" (cdr pair))
                     collect (cons (car pair)
                                   (string-trim-right (cdr pair)
                                                      "\\(?:>\\|]\\|}\\)+\\'"))))))

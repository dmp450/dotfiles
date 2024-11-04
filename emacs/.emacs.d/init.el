;; Set emacs default stuff
(setq inhibit-startup-message t)	; Disable the startup tutorial screen
(setq sentence-end-double-space nil)	; Single space should end a sentence.
(setq-default fill-column 80)
(setq column-number-mode t)		; Enable column number mode 
;; (add-hook 'text-mode-hook 'auto-fill-mode)
(setq initial-scratch-message "Welcome to Emacs :-)\n")
(setq ring-bell-function 'ignore)
(setq delete-old-versions -1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))) ; set a backups directory
(setq session-save-dir "~/emacs/sessions/")
(setq tramp-default-method "ssh")
(setq find-file-visit-truename t)
(setq native-comp-async-report-warnings-errors 'silent)
(defalias 'yes-or-no-p 'y-or-n-p)	; change yes/no prompts to y/n
(add-to-list 'exec-path "/home/dperrin/.bin")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(set-frame-font "Source Code Pro")

;; Start Emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; Enable global scroll precision mode
(pixel-scroll-precision-mode 1)

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
;; (straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; load configuration files
(require 'init-org)
(require 'init-browser)
(require 'init-chat)
(require 'init-elfeed)
(require 'init-git)
(require 'init-latex)
(require 'texpresso)
(require 'init-mail)
(require 'init-markdown)
(require 'init-navigation)
(require 'init-programming)
(require 'init-themes)
(require 'init-yasnippet)
(require 'init-lsp)

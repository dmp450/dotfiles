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
  ;; automatically start mu4e
  (add-hook 'after-init-hook #'mu4e))

(provide 'init-mail)

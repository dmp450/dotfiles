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

(use-package telega
  :load-path "~/.emacs.d/straight/repos/telega.el/"
  :custom
  (telega-sticker-animated-play "/home/dperrin/.bin/tgs2png")
  :ensure t)

(provide 'init-chat)

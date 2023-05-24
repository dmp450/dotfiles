(use-package org
  :straight (:type built-in)
  :demand t
  :config
  (setq org-adapt-indentation t)
  (add-hook 'org-mode-hook (lambda ()
	  (setq-local electric-pair-inhibit-predicate
		      `(lambda (c)
			 (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(provide 'init-org)

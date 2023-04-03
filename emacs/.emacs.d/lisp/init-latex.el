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
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
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

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))

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

(provide 'init-latex)

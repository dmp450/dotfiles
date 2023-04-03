(use-package go-mode
  :mode "\\*\\.go")

;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein)

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

(use-package rust-mode)

(use-package sage-shell-mode
  :ensure t
  :config
  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode))

(provide 'init-programming)

(require 'use-package)

(use-package flycheck
  :ensure t
  :init
  (progn
    (global-flycheck-mode t)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                  flycheck-emacs-lisp-load-path 'inherit)))

(provide 'my-flycheck)

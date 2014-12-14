(require 'use-package)

(use-package yasnippet
  :ensure t
  :commands yas-global-mode
  :init
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1)
    (defun disable-final-newline ()
      (set (make-local-variable 'require-final-newline) nil))
    (add-hook 'snippet-mode-hook 'disable-final-newline)))

(provide 'my-yasnippet)

(require 'use-package)

(use-package diff-hl
  :ensure t
  :config
  (progn
    (global-diff-hl-mode t)))

(provide 'my-diff-hl)

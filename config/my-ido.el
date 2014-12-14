(require 'use-package)

(use-package ido
  :ensure t
  :config
  (progn
    (ido-mode t)
    (setq ido-enable-flex-matching t
          ido-everywhere t
          ido-auto-merge-work-directories-length -1)

    (use-package ido-ubiquitous
      :ensure t
      :config
      (progn
        (ido-ubiquitous-mode t)))

    (use-package smex
      :ensure t
      :bind ("M-s" . smex))))

(provide 'my-ido)

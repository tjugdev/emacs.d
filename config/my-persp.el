(require 'use-package)

(use-package perspective
  :ensure t
  :config
  (progn
    (persp-mode)
    (require 'persp-projectile)))

(provide 'my-persp)

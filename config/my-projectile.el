(require 'use-package)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-enable-caching t)
    (projectile-global-mode t)
    (setq projectile-globally-ignored-directories '(".git"
                                                    ".svn"
                                                    "elpa"
                                                    "node_modules"))
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-indexing-method 'alien)))

(provide 'my-projectile)

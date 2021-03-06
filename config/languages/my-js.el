(require 'use-package)

(declare-function evil-define-key "evil-core")

(use-package js2-mode
  :ensure t
  :config
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (setq js2-highlight-level 3)))

(use-package tern
  :ensure t
  :config
  (progn
    (defun my/setup-tern ()
      (tern-mode t))
    (add-hook 'js-mode-hook 'my/setup-tern)

    (eval-after-load 'evil
      '(progn
         (evil-define-key 'normal js-mode-map (kbd "M-.") 'tern-find-definition)))

    (use-package tern-auto-complete
      :ensure t
      :config
      (progn
        (tern-ac-setup)))))

(provide 'my-js)

(require 'use-package)

(declare-function evil-define-key "evil-core")

(use-package auto-complete
  :ensure t
  :config
  (progn
    (require 'auto-complete-config)
    (ac-config-default)

    (setq ac-auto-show-menu t)
    (setq ac-auto-start t)
    (setq ac-show-menu-immediately-on-auto-complete t)
    (setq ac-use-menu-map t)

    (defun my/yasnippet-ac-integration ()
      "Disable autocomplete when able to expand a snippet.  Otherwise, autocomplete."
      (add-hook 'yas-before-expand-snippet-hook (lambda () (auto-complete-mode -1)))
      (add-hook 'yas-after-exit-snippet-hook (lambda () (auto-complete-mode t)))
      (defadvice ac-expand (before advice-for-ac-expand activate)
        (setq yas-fallback-behavior nil)
        (when (yas-expand)
          (ac-stop))))

    (eval-after-load 'yasnippet
      '(my/yasnippet-ac-integration))

    (eval-after-load 'evil
      '(progn
         (evil-define-key 'insert ac-menu-map "\C-n" 'ac-next)
         (evil-define-key 'insert ac-menu-map "\C-p" 'ac-previous)))))

(provide 'my-autocomplete)

(require 'use-package)

(declare-function evil-define-key "evil-core")

(use-package auto-complete
  :ensure t
  :config
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    ;; disable auto-start so that yasnippets plays nicely with auto-complete
    (setq ac-auto-start nil)
    (setq ac-use-comphist nil)
    (setq ac-dwim nil)
    (setq ac-use-menu-map t)

    (ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<tab>")

    (eval-after-load 'evil
      '(progn
         (evil-define-key 'insert ac-menu-map "\C-n" 'ac-next)
         (evil-define-key 'insert ac-menu-map "\C-p" 'ac-previous)))))

(provide 'my-autocomplete)

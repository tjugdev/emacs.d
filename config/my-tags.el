(require 'use-package)

(declare-function evil-define-key "evil-core")

(use-package ggtags
  :ensure t
  :init
  (progn
    (defun my/setup-ggtags ()
      (when (derived-mode-p 'c-mode 'c++-mode)
        (ggtags-mode 1)))

    (add-hook 'c-mode-common-hook 'my/setup-ggtags)
    (eval-after-load 'evil
      '(progn
         (evil-define-key 'normal c++-mode-map (kbd "M-.") 'ggtags-find-tag-dwim)))))

(provide 'my-tags)

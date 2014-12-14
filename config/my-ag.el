(require 'use-package)

(use-package ag
  :ensure t
  :defer t
  :init
  (progn
    (defun my/setup-ag ()
      (switch-to-buffer-other-window "*ag search*"))
    (add-hook 'ag-mode-hook 'my/setup-ag))
  :config
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (define-key ag-mode-map (kbd "k") nil)))

(provide 'my-ag)

(require 'use-package)

(use-package ido
  :ensure t
  :config
  (progn
    (ido-mode t)
    (setq ido-enable-flex-matching t
          ido-everywhere t
          ido-auto-merge-work-directories-length -1
          ido-save-directory-list-file (concat user-emacs-directory ".saves"))

    (defun my/ido-jump-to-home ()
      (interactive)
      (ido-set-current-directory "~/")
      (setq ido-exit 'refresh)
      (exit-minibuffer))

    (defun my/setup-ido ()
      (define-key ido-file-dir-completion-map "~" 'my/ido-jump-to-home))

    (add-hook 'ido-setup-hook 'my/setup-ido)

    (use-package ido-ubiquitous
      :ensure t
      :config
      (progn
        (ido-ubiquitous-mode t)))

    (use-package smex
      :ensure t
      :bind ("M-s" . smex))))

(provide 'my-ido)

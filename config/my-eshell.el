(require 'use-package)

(declare-function evil-set-initial-state "evil-core")

(defun my/toggle-eshell-buffer ()
  "Show/hide eshell buffer"
  (interactive)
  (let ((eshell-window (get-buffer-window "*eshell*")))
    (if eshell-window
        (delete-window eshell-window)
      (progn
        (select-window (split-window-below -20))
        (eshell)))))

(eval-after-load 'evil
  '(progn
     (evil-set-initial-state 'eshell-mode 'emacs)))
(defun my/setup-eshell ()
  "Set up my eshell config"
  (linum-mode -1)
  (setq show-trailing-whitespace -1))

(global-set-key (kbd "<f8>") 'my/toggle-eshell-buffer)

(add-hook 'eshell-mode-hook 'my/setup-eshell)

(provide 'my-eshell)

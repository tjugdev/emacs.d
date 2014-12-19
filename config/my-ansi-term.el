(require 'use-package)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-set-initial-state "evil-core")

(defun my/visit-ansi-term-buffer ()
  "Show/hide ansi-term buffer"
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (select-window (split-window-sensibly (selected-window)))
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(eval-after-load 'yasnippet
  '(progn
        (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))))

(defun my/setup-ansi-term ()
  "Set up my ansi-term config"
  (setq comint-move-point-for-output t)
  (linum-mode -1)
  (setq show-trailing-whitespace nil))

(defadvice term-sentinel (around my/advice-term-sentinel (proc msg) activate)
  "Kill ansi-term buffer when process dies"
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer)
        (delete-window))
    ad-do-it))

(global-set-key (kbd "C-c t") 'my/visit-ansi-term-buffer)

(add-hook 'term-mode-hook 'my/setup-ansi-term)

(provide 'my-ansi-term)

(require 'use-package)

(defun my/toggle-eshell-buffer ()
  "Show/hide eshell buffer"
  (interactive)
  (let ((eshell-window (get-buffer-window "*eshell*")))
    (if eshell-window
        (delete-window eshell-window)
      (progn
        (select-window (split-window-below -20))
        (eshell)))))

(global-set-key (kbd "<f8>")  'my/toggle-eshell-buffer)

(declare-function evil-set-initial-state "evil-core")
(eval-after-load "evil"
  '(progn
     (evil-set-initial-state 'eshell-mode 'emacs)))

(provide 'my-eshell)

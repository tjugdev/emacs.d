(require 'use-package)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key "evil-core")

;; Rather than setting magit modes to evil normal states, I keep them in emacs
;; state and selectively add a few vim-like bindings where they make sense
(use-package magit
  :ensure t
  :config
  (progn
    ;; This is my M-x and it's already defined as 4 as well
    (define-key magit-mode-map (kbd "M-s") nil)

    (eval-after-load 'evil
      '(progn
         (evil-set-initial-state 'magit-mode 'emacs)
         (evil-set-initial-state 'git-commit-mode 'insert)))))

(provide 'my-magit)

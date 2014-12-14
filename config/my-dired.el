(require 'use-package)

(use-package dired-x
  :init
  (progn
    (toggle-diredp-find-file-reuse-dir 1)))

(provide 'my-dired)

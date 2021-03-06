;; -*- lexical-binding: t -*-
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "config/languages"))

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; cleaner ui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; eyecandy
(set-face-attribute 'default nil :height 90)
(load-theme (if (package-installed-p 'color-theme-solarized)
                'solarized-dark
              'wombat)
            t)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))

;; various tweaks
(setq require-final-newline t)
(show-paren-mode t)
(global-linum-mode t)
(global-font-lock-mode t)
(require 'saveplace)
(setq-default save-place t)
(setq scroll-conservatively 999
      scroll-preserve-screen-position t)
(setq make-backup-files t
      backup-directory-alist `(("." . "~/.emacs.d/.saves"))
      backup-by-copying t
      auto-save-default nil)

;; No tabs!
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(require 'my-ido)
(require 'my-dired)
(require 'my-eshell)
(require 'my-ansi-term)
(require 'my-flycheck)
(require 'my-ag)
(require 'my-autocomplete)
(require 'my-yasnippet)
(require 'my-magit)
(require 'my-languages)
(require 'my-tags)
(require 'my-projectile)
(require 'my-persp)
(require 'my-diff-hl)

(use-package evil-leader
  :commands (evil-leader-mode global-evil-leader-mode)
  :ensure t
  :demand evil-leader
  :init
  (progn
    (global-evil-leader-mode t))
  :config
  (progn
    (evil-leader/set-key
      (kbd "<") 'persp-prev
      (kbd ">") 'persp-next
      (kbd "a") 'ag-project-regexp
      (kbd "c") 'projectile-persp-switch-project
      (kbd "d") 'dired-jump
      (kbd "f") 'ido-find-file
      (kbd "gl") 'magit-log
      (kbd "gs") 'magit-status
      (kbd "gc") 'magit-commit
      (kbd "p") 'projectile-find-file
      (kbd "P") 'projectile-switch-project)))

(use-package evil
  :ensure t
  :commands (evil-set-initial-state)
  :config
  (progn
    (evil-mode 1)

    (use-package evil-nerd-commenter
      :ensure t
      :commands (evilnc-comment-or-uncomment-lines))

    ;; Use , as a window command prefix
    (mapc (lambda (state)
            (define-key state (kbd ",") nil)
            (define-key state (kbd ",,") 'evil-repeat-find-char-reverse)
            (define-key state (kbd ",h") 'evil-window-left)
            (define-key state (kbd ",l") 'evil-window-right)
            (define-key state (kbd ",k") 'evil-window-up)
            (define-key state (kbd ",j") 'evil-window-down)
            (define-key state (kbd ",c") 'evil-window-delete)
            (define-key state (kbd ",o") 'delete-other-windows)
            (define-key state (kbd ",H") 'evil-window-move-far-left)
            (define-key state (kbd ",L") 'evil-window-move-far-right)
            (define-key state (kbd ",K") 'evil-window-move-very-top)
            (define-key state (kbd ",J") 'evil-window-move-very-bottom))
          (list evil-normal-state-map evil-motion-state-map magit-mode-map))
    (define-key evil-normal-state-map (kbd "SPC") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd "SPC") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map (kbd "]c") 'diff-hl-next-hunk)
    (define-key evil-normal-state-map (kbd "[c") 'diff-hl-previous-hunk)

    ;; Use ido to open files
    (defun my/ido-evil-file-cmd (cmd)
      (lambda (file)
        (interactive (list (ido-read-file-name "")))
        (funcall cmd nil file)))
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)
    (define-key evil-ex-map "sp " (my/ido-evil-file-cmd 'evil-window-split))
    (define-key evil-ex-map "vsp " (my/ido-evil-file-cmd 'evil-window-vsplit))
    ))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "<escape>") 'keyboard-quit)

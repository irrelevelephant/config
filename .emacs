(setq inhibit-startup-message t)
(menu-bar-mode -1)
(setq backup-inhibited t)
(setq auto-save-default nil)

(icomplete-mode 99)
(global-auto-revert-mode 1)
(global-set-key "\C-x\C-b" 'buffer-menu)
(put 'dired-find-alternate-file 'disabled nil)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-auto-revert-mode t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(add-hook 'markdown-mode-hook #'visual-line-mode)

(add-hook 'scss-mode-hook
    (lambda ()
        (setq c-basic-offset 2)
        (setq indent-tabs-mode nil)))

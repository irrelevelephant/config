; general
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(global-auto-revert-mode t)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(delete-selection-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
(setq find-file-visit-truename t)
(setq create-lockfiles nil)
(setq debug-on-error t)

(setenv "BASH_ENV" "~/.bashrc")

(setq-default find-args "-type f ! -path './node_modules/*' ! -path '*/\.*'")

; customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

; custom keybindings
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-c s") 'sort-lines)

; page break lines
(use-package page-break-lines
  :ensure
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

; ignored files
(setq global-ignored-files '(".git" "build" "eclipse-bin"))

; help
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

; prompt for close
(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? "))
      (save-buffers-kill-terminal)
    (message "Canceled frame close")))

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

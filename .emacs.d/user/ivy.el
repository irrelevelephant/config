(defun counsel-projectile-rg-no-test ()
  (interactive)
  (counsel-projectile-rg "-g !tst/"))

; ivy
(use-package ivy
  :ensure
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq counsel-rg-base-command
        "rg -i -M 240 --no-heading --line-number --color never %s || true")
  :bind
  ("C-c C-r" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g" . counsel-git)
  ("C-c h" . counsel-git-grep)
  ("C-c k" . counsel-projectile-rg)
  ("C-c j" . counsel-projectile-rg-no-test)
  ("C-c f" . projectile-find-file)
  (:map minibuffer-local-map ("C-r" . counsel-minibuffer-history)))

(use-package counsel
  :ensure
  :diminish counsel-mode
  :config
  (counsel-mode))

(use-package swiper
  :ensure
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy-rich
  :ensure
  :after ivy
  :config
  (ivy-rich-mode 1)
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

; smex
(use-package smex
  :ensure
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

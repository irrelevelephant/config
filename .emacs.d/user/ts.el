(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; Configure flycheck
(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode))

;; Configure web-mode for .tsx files
(use-package web-mode
  :ensure t
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (tide-mode)))))

;; Configure tide mode
(use-package tide
  :ensure t
  :defer t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide-mode)
         (web-mode . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (setup-tide-mode)))))
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)))

;; Configure typescript-mode
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . tide-mode))

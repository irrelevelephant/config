; customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

; packages
(add-to-list 'load-path "~/.emacs.d/elisp/")
(setq package-check-signature nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package diminish)

(defun recompile-packages ()
    (interactive)
    (byte-recompile-directory package-user-dir nil 'force))

; general
(setq inhibit-startup-message t)
(menu-bar-mode -1)
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

(setq debug-on-error t)

(setenv "BASH_ENV" "~/.bashrc")

; themes
(setq doom-theme-name 'doom-vibrant)
(use-package doom-themes
 :config
 (load-theme doom-theme-name t)
 (doom-themes-org-config)
 (doom-themes-set-faces doom-theme-name
   '(default :background "black")))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq
   doom-modeline-buffer-file-name-style 'truncate-with-project
   doom-modeline-minor-modes t
   doom-modeline-lsp t
   inhibit-compacting-font-caches t))

; prog mode
(global-display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

; custom keybindings
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-c s") 'sort-lines)

; buffer switching
(global-set-key "\C-x\C-b" 'buffer-menu)

; window management
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(winner-mode 1)

; popwin
(use-package popwin
  :config
  (popwin-mode 1)
  :init
  (setq popwin:special-display-config nil)
  (push '("*Help*" :stick t :position bottom) popwin:special-display-config)
  (push '("*Backtrace*" :stick t :position bottom) popwin:special-display-config)
  (push '("*Compile-Log*" :stick t :position bottom) popwin:special-display-config)
  (push '(*grep* :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config))

; autocomplete
(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  :init
  (setq-default ac-use-menu-map t)
  (setq ac-auto-show-menu nil))

; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

; smart parens
(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(bind-keys
  :map smartparens-mode-map
  ("C-M-a" . sp-beginning-of-sexp)
  ("C-M-e" . sp-end-of-sexp)

  ("C-M-f" . sp-forward-sexp)
  ("C-M-b" . sp-backward-sexp)

  ("C-M-n" . sp-next-sexp)
  ("C-M-p" . sp-previous-sexp)

  ("C-M-[" . sp-backward-unwrap-sexp)
  ("C-M-]" . sp-unwrap-sexp)

  ("C-M-t" . sp-transpose-sexp)
  ("C-M-k" . sp-kill-sexp))

; code folding
(use-package hideshow
  :diminish hs-minor-mode
  :hook ((prog-mode . hs-minor-mode)))

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))

(bind-keys
  :map prog-mode-map
  ("C-M-h" . toggle-fold))

; compile mode
(ignore-errors
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate '(lambda () nil))
(setq compilation-scroll-output t)

; page break lines
(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

; ivy
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :bind
  ("C-c C-r" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-rg)
  (:map minibuffer-local-map ("C-r" . counsel-minibuffer-history)))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

; smex
(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

; ignored files
(setq global-ignored-files '(".git" "build" "eclipse-bin"))

; git
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch)
  :config
  (set-face-attribute 'magit-diff-added-highlight nil :background "color-41" :foreground "brightwhite")
  (set-face-attribute 'magit-diff-context-highlight nil :background "brightblack" :foreground "brightwhite")
  (set-face-attribute 'magit-diff-removed-highlight nil :background "color-197" :foreground "brightwhite")
  (set-face-attribute 'magit-section-highlight nil :background "color-34" :foreground "brightwhite"))

; projectile
(use-package projectile
  :after counsel
  :demand
  :config
  (projectile-register-project-type 'brazil '("Config")
                                    :compile "bb")
  (projectile-register-project-type 'java '(".project")
                                    :compile "bb"
                                    :test-suffix "Test")
  (setq projectile-globally-ignored-directories
        (append global-ignored-files
                projectile-globally-ignored-directories))
  (add-hook 'projectile-mode 'counsel-projectile-mode)
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'ivy)
  :bind
  (:map projectile-mode-map ("M-p" . projectile-command-map)))

; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode))

; help
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

; xml
(setq nxml-child-indent 4 nxml-attribute-indent 4)

; ruby
(setq ruby-indent-level 2)

; yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.template\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook 'auto-complete-mode))

; json
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)
            (setq truncate-lines t)))

; javascript
(setq js-indent-level 2)

; webmode
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode)
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . web-mode))))

; jinja
(use-package jinja2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode)))

; markdown
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode))

; csharp
(defun my-csharp-mode-setup ()
  (subword-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(use-package lsp-ui)
(use-package lsp-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'csharp-mode-hook #'lsp)
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  :custom
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold nil)
  (lsp-file-watch-ignored
   '(".idea"
     "node_modules"
     ".git"
     "target"
     "build"
     "env"
     "logs")))

(use-package hydra)
(use-package company-lsp
  :diminish company-mode
  :custom
  (company-lsp-cache-candidates 'auto)
  (company-idle-delay 0)
  (company-dabbrev-downcase 0)
  :bind
  (:map company-active-map
        ("TAB" . company-complete-selection)
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort)))
(use-package flycheck
  :after lsp-mode)

;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (require 'dap-java)
;;   (dap-mode t)
;;   (dap-ui-mode t))

;; (define-key java-mode-map (kbd "M-i") 'lsp-java-organize-imports)
;; (defalias 'tc 'dap-java-run-test-class)
;; (defalias 'tm 'dap-java-run-test-method)

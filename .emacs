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
(setq create-lockfiles nil)

(setq debug-on-error t)

(setenv "BASH_ENV" "~/.bashrc")

(setq-default find-args "-type f ! -path './node_modules/*' ! -path '*/\.*'")

; themes
(setq doom-theme-name 'doom-vibrant)
(use-package doom-themes
 :config
 (load-theme doom-theme-name t)
 (doom-themes-org-config)
 (custom-set-faces
  '(default ((t (:background "#000000"))))))

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

; paredit
(use-package paredit
  :config
    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

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
  (setq counsel-grep-base-command
        "rg -i -M 240 --no-heading --line-number '%s' %s")
  :bind
  ("C-c C-r" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-projectile-rg)
  ("C-c f" . projectile-find-file)
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
(use-package counsel-projectile)

(use-package projectile
  :after counsel
  :demand
  :config
  (projectile-register-project-type 'npm '("package.json")
    :project-file "package.json")
  (defcustom projectile-project-root-functions
    '(projectile-root-local
      projectile-root-bottom-up
      projectile-root-top-down
      projectile-root-top-down-recurring)
    "A list of functions for finding project roots."
    :group 'projectile
    :type '(repeat function))
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

; bash
(add-hook 'sh-mode-hook
  (lambda ()
    (setq truncate-lines t)))

; json
(add-hook 'json-mode-hook
  (lambda ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)
    (setq truncate-lines t)))

; javascript
(add-hook 'js-mode-hook
  (lambda ()
    (subword-mode)
    (setq truncate-lines t)
    (setq indent-tabs-mode nil)
    (setq js-indent-level 4)
    (setq js-switch-indent-offset 4)))

; typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq company-tooltip-align-annotations t)
  (subword-mode)
  (setq truncate-lines t)
  (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver"))
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

; tsx
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'web-mode)

; graphql
(use-package graphql-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.graphqls\\'" . graphql-mode))
  (add-hook 'graphql-mode-hook
     (lambda() (subword-mode))))

; mmm
(use-package mmm-mode
  :diminish mmm-mode)

(mmm-add-classes
    '((js-graphql
          :submode graphql-mode
          :face mmm-declaration-submode-face
          :front "[^a-zA-Z]gql`" ;; regex to find the opening tag
          :back "`"))) ;; regex to find the closing tag
(mmm-add-mode-ext-class 'js-mode nil 'js-graphql)
(mmm-add-mode-ext-class 'typescript-mode nil 'js-graphql)
(setq mmm-global-mode 'maybe)
;; Optional configuration that hides the background color for a highlighted block
(setq mmm-submode-decoration-level 0)

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
  (add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode)))

; jinja
(use-package jinja2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode)))

; markdown
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode))

; docker
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

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

; go
(use-package go-mode
  :config
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  (add-hook 'rustic-mode-hook #'smartparens-mode)
  (add-hook 'rustic-mode-hook 'subword-mode))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck
  :ensure
  :diminish)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	       :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))

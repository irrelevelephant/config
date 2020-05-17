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
(iswitchb-mode 1)
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
  ("C-c k" . counsel-ag)
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

;json
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)
            (setq truncate-lines t)))

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
(use-package company)

(use-package omnisharp
  :config
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t))

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

; java
(add-hook 'java-mode-hook (lambda () (setq truncate-lines t)))

(defun java-prefs ()
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-intro 8)
  (c-set-offset 'arglist-cont-nonempty 8)
  (c-set-offset 'statement-cont 8)
  (c-set-offset 'annotation-var-cont 0)
  (c-set-offset 'func-decl-cont 8))
(add-hook 'java-mode-hook 'java-prefs)

; java lsp
;; (use-package lsp-mode
;;   :config
;;   (require 'lsp-ui)
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   :custom
;;   (lsp-session-file nil)
;;   (lsp-prefer-flymake nil)
;;   (lsp-file-watch-threshold nil)
;;   (lsp-file-watch-ignored
;;    '(".idea"
;;      "node_modules"
;;      ".git"
;;      "target"
;;      "build"
;;      "env"
;;      "logs"))
;;   (lsp-java-import-exclusions
;;     '("**/node_modules/**"
;;       "**/.metadata/**"
;;       "**/build"
;;       "**/META-INF/maven/**"))
;;   (lsp-ui-sideline-enable nil))

;; (use-package hydra)
;; (use-package company-lsp
;;   :diminish company-mode
;;   :custom
;;   (company-lsp-cache-candidates 'auto)
;;   (company-idle-delay 0)
;;   (company-dabbrev-downcase 0)
;;   :bind
;;   (:map company-active-map
;;         ("TAB" . company-complete-selection)
;;         ("C-n" . company-select-next-or-abort)
;;         ("C-p" . company-select-previous-or-abort)))
;; (use-package lsp-java
;;   :after lsp
;;   :custom
;;   (lsp-java-vmargs
;;    '("-noverify"
;;      "-Xmx1G"
;;      "-XX:+UseG1GC"
;;      "-XX:+UseStringDeduplication"
;;      "-javaagent:/home/thmssmth/.emacs.d/lombok.jar"
;;      "-Xbootclasspath/a:/home/thmssmth/.emacs.d/lombok.jar"))
;;   (lsp-file-watch-ignored global-ignored-files)
;;   (lsp-java-format-settings-url "/home/thmssmth/.emacs.d/eclipse-starling-java-code-style.xml"))

;; (use-package flycheck
;;   :after lsp-mode
;;   :hook lsp-java)

;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (require 'dap-java)
;;   (dap-mode t)
;;   (dap-ui-mode t))

;; (defun my/lsp ()
;;   (interactive)
;;   (let* ((brazil-session-file
;;           (apply 'f-join (append (-slice (f-split (projectile-project-root)) 0 -2) '(".lsp-session-v1")))))
;;     (when (not (f-exists? brazil-session-file)) (write-region "" nil brazil-session-file))
;;     (when (and lsp-session-file (not (f-same? lsp-session-file brazil-session-file)))
;;       (error (format "Session %s already open" lsp-session-file)))
;;     (when (not lsp-session-file) (setq lsp--session nil))
;;     (setq lsp-session-file brazil-session-file)
;;     (message "Using LSP session at %s" lsp-session-file)
;;     (lsp-deferred)))
;; (add-hook 'java-mode-hook #'my/lsp)

;; (add-hook 'java-mode-hook
;;           (lambda() (subword-mode)))

;; (define-key java-mode-map (kbd "M-i") 'lsp-java-organize-imports)
;; (defalias 'tc 'dap-java-run-test-class)
;; (defalias 'tm 'dap-java-run-test-method)

; javap
(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))

(defun javap-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
        (call-process "javap" nil (current-buffer) nil "-verbose"
                      "-classpath" (file-name-directory file)
                      (file-name-sans-extension
                       (file-name-nondirectory file)))
        (setq buffer-file-name file)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (java-mode)
        (current-buffer))))
   ((javap-handler-real op args))))

(defun javap-handler-real (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'javap-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

; sim
;(add-to-list 'load-path "~/Emacs-org-issues-mode/src")
;(require 'org-issues-mode)
;(org-issues-update/monitor-issues) ;; Sets up a timer to automatically keep your local Issues up-to-date

(setq js-indent-level 2)

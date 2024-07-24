(global-display-line-numbers-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (progn
              (setq show-trailing-whitespace t))))

; subword
(global-subword-mode 1)

; rainbow delimiters
(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

; smart parens
(use-package smartparens
  :ensure
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

; wrapping
(add-hook 'prog-mode-hook (lambda ()
                            (visual-line-mode -1)
                            (setq truncate-lines t)))

; compile mode
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (interactive)
  (when (or (eq major-mode 'compilation-mode) (eq major-mode 'dap-server-log-mode))
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'dap-server-log-mode 'colorize-compilation-buffer)

(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate '(lambda () nil))
(setq compilation-scroll-output t)

; markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init (setq markdown-command "pandoc"))

; yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
        ("\\.yaml\\'" . yaml-mode))

; docker
(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

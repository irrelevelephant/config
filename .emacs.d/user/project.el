; git
(use-package magit
  :ensure
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch)
  ("C-c p" . magit-find-file)
  :config
  (set-face-attribute 'magit-diff-added nil :background "green" :foreground "black")
  (set-face-attribute 'magit-diff-removed nil :background "red" :foreground "black")

  (set-face-attribute 'magit-diff-added-highlight nil :background "brightgreen" :foreground "black")
  (set-face-attribute 'magit-diff-removed-highlight nil :background "brightred" :foreground "black")

  (set-face-attribute 'magit-diff-context-highlight nil :background "brightblack" :foreground "brightwhite")
  (set-face-attribute 'magit-section-highlight nil :background "brightblack" :foreground "brightwhite"))

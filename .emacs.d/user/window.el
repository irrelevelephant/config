; window management
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(winner-mode 1)

(global-set-key (kbd "M-[ a") 'windmove-up)
(global-set-key (kbd "M-[ b") 'windmove-down)
(global-set-key (kbd "M-[ c") 'windmove-right)
(global-set-key (kbd "M-[ d") 'windmove-left)

(define-key comint-mode-map (kbd "M-O a") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-O b") 'comint-next-input)

(tab-bar-mode 1)
(setq-default tab-bar-close-button-show nil)

; popwin
(use-package popwin
  :ensure
  :config
  (popwin-mode 1)
  :init
  (setq popwin:special-display-config nil)
  (push '("*Help*" :stick t :position bottom) popwin:special-display-config)
  (push '("*Backtrace*" :stick t :position bottom) popwin:special-display-config)
  (push '("*Compile-Log*" :stick t :position bottom) popwin:special-display-config)
  (push '(*grep* :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config))

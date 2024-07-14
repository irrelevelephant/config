(setq gc-cons-threshold (* 10 1000 1000))

(setq custom-file (concat user-emacs-directory "custom.el"))

(package-initialize)
(require 'package)
(add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t)

(dolist (package '(use-package quelpa quelpa-use-package))
   (unless (package-installed-p package)
     (progn
       (package-refresh-contents)
       (package-install package))))

(require 'use-package)
(require 'quelpa-use-package)

(use-package diminish :ensure)

(defvar user-elisp-dir (expand-file-name "~/.emacs.d/user"))

(defun byte-compile-user-elisp-files ()
  "Byte-compile all Elisp files in `user-elisp-dir`."
  (when (file-directory-p user-elisp-dir)
    (byte-recompile-directory user-elisp-dir 0)))

(byte-compile-user-elisp-files)

(defun load-user-elisp-files ()
  "Load all byte-compiled Elisp files in `user-elisp-dir`."
  (when (file-directory-p user-elisp-dir)
    (let ((load-it (lambda (f)
                     (load-file (concat (file-name-as-directory user-elisp-dir) f)))))
      (mapc load-it (directory-files user-elisp-dir nil "\\.elc$")))))

(load-user-elisp-files)

;; (use-package esup
;;   :ensure t)

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; add settings to the load-path
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))

;; default config set
(setq my-config-set 'default)

;; load machine local settings
(setq my-local-init (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p my-local-init)
      (load-file my-local-init))

;; load config set
(load-file (expand-file-name
            (concat (file-name-as-directory "config-sets")
                    (concat (symbol-name my-config-set) ".el"))
            user-emacs-directory))

;; key bindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f10>") 'magit-status)

;; use UTF-8 with unix line endings
(setq-default buffer-file-coding-system 'utf-8-unix)

;; after ediff, put back windows as they were
(add-hook 'ediff-before-setup-hook (lambda () (window-configuration-to-register 'e)))
(add-hook 'ediff-quit-hook (lambda () (jump-to-register 'e)))

;; global modes
(global-linum-mode 1)
(electric-pair-mode 1)

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; GPII

(defun start-easit-oauth-integration ()
  (interactive)
  (let ((default-directory (concat (file-name-as-directory (getenv "GPII_UNIVERSAL_HOME"))
                                   (file-name-as-directory "examples")
                                   (file-name-as-directory "easit-oauth-integration"))))
    (async-shell-command "node driver.js" "*easit-oauth-integration*")))

(defun start-gpii-oauth2-sample-client ()
  (interactive)
  (let ((default-directory (concat (file-name-as-directory (getenv "GPII_UNIVERSAL_HOME"))
                                   (file-name-as-directory "examples")
                                   (file-name-as-directory "gpii-oauth2-sample-client"))))
    (async-shell-command "node app.js" "*gpii-oauth2-sample-client*")))

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-default-style
   (quote
    ((c-mode . "stroustrup")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(column-number-mode t)
 '(default-frame-alist (quote ((width . 120) (height . 56))))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fringe-mode (quote (0 . 8)) nil (fringe))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-global-externs (quote ("fluid" "jQuery" "require")))
 '(line-number-mode nil)
 '(linum-format "%4d  ")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

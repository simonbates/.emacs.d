;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; default config
(setq my-config-name 'default)

;; load machine local settings
(setq my-local-init (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p my-local-init)
      (load-file my-local-init))

;; load config
(load-file (expand-file-name
            (concat (file-name-as-directory "configs")
                    (concat (symbol-name my-config-name) ".el"))
            user-emacs-directory))

;; diminish
(require 'diminish)

;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(helm-mode 1)
(diminish 'helm-mode)

(setq helm-mode-no-completion-in-region-in-modes
      '(circe-channel-mode
        circe-query-mode
        circe-server-mode))

;; helm-gtags
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "C-c s") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "C-c f") 'helm-gtags-parse-file)))

;; projectile
(projectile-global-mode 1)
(require 'helm-projectile)
(helm-projectile-on)
;; bind projectile-grep rather than helm-projectile-grep
(define-key projectile-command-map (kbd "s g") 'projectile-grep)
(diminish 'projectile-mode)

;; org mode
(setq org-src-fontify-natively t)

;; org-present
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-read-write)))))

;; yasnippet
(require 'yasnippet)
; (yas-global-mode 1)

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; irony
(require 'irony)
(require 'irony-cdb)
(require 'irony-eldoc)
(add-hook 'irony-mode-hook
          (lambda ()
            (define-key irony-mode-map [remap completion-at-point]
              'irony-completion-at-point-async)
            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)
            (yas-minor-mode 1)
            (eldoc-mode 1)
            (irony-eldoc 1)))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; company
(require 'company)
(global-set-key (kbd "C-M-i") 'company-complete)
(diminish 'company-mode)

;; grep
(add-hook 'grep-mode-hook
          (lambda ()
            (setq truncate-lines t)))

;; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (company-mode 1)))

;; C
(defun my-c-mode-hook ()
  (company-mode 1)
  (irony-mode 1)
  (helm-gtags-mode 1))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; default to UTF-8 with unix line endings
(prefer-coding-system 'utf-8-unix)

;; after ediff, put back windows as they were
(add-hook 'ediff-before-setup-hook (lambda () (window-configuration-to-register 'e)))
(add-hook 'ediff-quit-hook (lambda () (jump-to-register 'e)))

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; global modes
;(global-linum-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode 1)
(size-indication-mode 1)

;; Circe

(setq circe-network-options
      (list (list "Freenode"
                  :nick my-freenode-nick
                  :realname my-freenode-realname
                  :channels '("#fluid-design" "#fluid-ops" "#fluid-tech" "#fluid-work"))))

(setq circe-use-cycle-completion t)

(add-hook 'circe-chat-mode-hook
          (lambda ()
            (lui-set-prompt
             (concat (propertize (concat (buffer-name) ">")
                                 'face 'circe-prompt-face)
                     " "))))

(setq lui-highlight-keywords '("fluid-everyone"))

(setq lui-flyspell-p t)
(setq lui-flyspell-alist '((".*" "american")))

(add-hook 'lui-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace nil)
            (setq-local global-hl-line-mode nil)))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Git commit with an empty message
(defun git-commit-with-empty-message ()
  (interactive)
  (if (y-or-n-p (concat "git commit " default-directory " with an empty message?"))
      (shell-command "git commit --allow-empty-message -m ''")))

;; git grep project
(defun git-grep-project ()
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (pattern (grep-read-regexp))
         (command (grep-expand-template "git --no-pager grep --untracked --line-number --ignore-case <R>" pattern)))
    (compilation-start command 'grep-mode)))

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

;; key bindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f7>") 'projectile-compile-project)
(global-set-key (kbd "<f8>") 'projectile-test-project)
(global-set-key (kbd "<f10>") 'magit-status)
(global-set-key (kbd "C-c g") 'git-grep-project)
(global-set-key (kbd "<M-up>") 'scroll-down-line)
(global-set-key (kbd "<M-down>") 'scroll-up-line)
(global-unset-key (kbd "C-v"))

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
 '(circe-highlight-nick-type (quote all))
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(default-frame-alist (quote ((width . 120) (height . 52))))
 '(delete-selection-mode nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fci-rule-color "#383838")
 '(gdb-many-windows t)
 '(helm-buffer-details-flag nil)
 '(helm-buffer-max-length nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-global-externs
   (quote
    ("__dirname" "fluid" "gpii" "jQuery" "jqUnit" "module" "require" "setTimeout")))
 '(line-number-mode t)
 '(linum-format "%4d ")
 '(magit-ediff-dwim-show-on-hunks t)
 '(menu-bar-mode nil)
 '(next-screen-context-lines 4)
 '(org-agenda-files (quote ("~/notes")))
 '(org-catch-invisible-edits (quote error))
 '(safe-local-variable-values
   (quote
    ((sgml-basic-offset . 2)
     (eval progn
           (when
               (require
                (quote projectile)
                nil t)
             (puthash
              (projectile-project-root)
              "cd build && make" projectile-compilation-cmd-map)
             (puthash
              (projectile-project-root)
              "cd build && ctest --output-on-failure" projectile-test-cmd-map))))))
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(scroll-step 0)
 '(sentence-end-double-space nil)
 '(sgml-basic-offset 4)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook
          (lambda ()
            (company-mode 1)))

(provide 'init-js2-mode)

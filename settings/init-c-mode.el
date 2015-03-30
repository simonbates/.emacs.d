(add-hook 'c-mode-common-hook
          (lambda ()
            (semantic-mode 1)
            (semantic-idle-summary-mode 1)
            (company-mode 1)
            (irony-mode 1)))

(provide 'init-c-mode)

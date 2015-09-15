(set-face-attribute 'default nil :font "Consolas-11")
(setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))
;; avoid the "GREP_OPTIONS is deprecated" warning
(require 'grep)
(grep-apply-setting 'grep-highlight-matches nil)

(set-face-attribute 'default nil :font "DejaVu Sans Mono-11")
(setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))
;; avoid the "GREP_OPTIONS is deprecated" warning
(require 'grep)
(grep-apply-setting 'grep-highlight-matches nil)

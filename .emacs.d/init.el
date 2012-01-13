; Congratulations! You're customizing your editor
;(add-to-list 'load-path "~/.emacs.d/")

; Add all top-level subdirectories of .emacs.d to the load path
;(progn (cd "~/.emacs.d")
;       (normal-top-level-add-subdirs-to-load-path))

; Default behavior
(load-library "~/.emacs.d/default")

; Third party libraries are stored in ~/.emacs.d/extern
(add-to-list 'load-path "~/.emacs.d/extern")
(progn (cd "~/.emacs.d/extern")
       (normal-top-level-add-subdirs-to-load-path))

; Python-specific enhancements
(load-library "~/.emacs.d/python")


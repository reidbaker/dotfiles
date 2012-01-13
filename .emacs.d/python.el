; use tabs in files (urgh...yelp!)
(setq-default indent-tabs-mode t)

; tab display width of 4 columns by default
; (throw everything at the wall, and eventually something will stick...)
(setq-default tab-width 4)  ; Normal emacs tab-width
; (setq-default c-basic-offset 2) ; python-mode.el setting
(setq-default py-indent-offset 4) ; Use Tabs, not spaces
(setq-default py-smart-indentation nil) ; Don't try to guess tab width

(defun customize-py-tabs ()
    (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode t
        py-smart-indentation nil
        python-indent 4
    )
 )
; end nasty tab
(add-hook 'python-mode-hook 'customize-py-tabs)

; Highlight useless whitespace
(add-hook 'python-mode-hook
                  (lambda () (setq show-trailing-whitespace t)))

; Flymakey things 
(require 'flymake)
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))
(add-hook 'python-mode-hook '(lambda () (customize-py-tabs)))
 
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
;
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

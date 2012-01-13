; This is for default behavior 

; Line and Column info
(setq-default line-number-mode t)
(setq-default column-number-mode t)

; For those heathen times when I want to scroll and click
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; highlight current line so that we don't get lost...
;; which happens a lot!
(global-hl-line-mode 1)
(set-face-background 'hl-line "blue")

;; comments 
(set-face-foreground 'font-lock-comment-face "red")

;; frickin backspace character on a mac
(global-set-key [(control h)] 'delete-backward-char)
(define-key key-translation-map [?\C-h] [?\C-?])
 
;; I love this bad-boy colour-scheme
(add-to-list 'load-path "~/.emacs.d/extern/")
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(load-file "~/.emacs.d/extern/color-theme-twilight.el")
(color-theme-twilight)
 
;; increase the font size slightly
(set-face-attribute 'default nil :height 100)
 
;; because I don't understand all of this mac-command business
(setq mac-command-modifier 'alt
       mac-option-modifier 'meta)

;; html-mode should conform to stuff with tabs ..ugh
(add-hook 'html-mode-hook
          (lambda()
            (setq tab-width 4)
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))

;; Says files that end in .tmpl should go to html mode
(add-to-list 'auto-mode-alist '("\\.tmpl$" . html-mode))

;; ctrl+tab inserts a hard tab. for times when auto indent is dumb
(defun hard-tab ()
  (interactive)
  (insert-char 9 1)
)
(global-set-key [C-tab] (lambda () (interactive) (insert-char 9 1) ))
(global-set-key [C-tab] 'hard-tab)
(global-set-key [C-tab] (lambda () (interactive) (insert-char 9 1) ))
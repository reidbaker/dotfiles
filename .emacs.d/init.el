;; package setup
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; things I want installed
(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      smex
                      pyflakes
                      pony-mode
                      paredit
                      magit
                      ido-ubiquitous
                      idle-highlight
                      git-commit
                      flymake
                      flymake-css
                      flymake-jshint
                      find-file-in-project)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; color theme and font
(set-face-attribute 'default nil :family "Monospace" :height 100)

;; ibuffer as default
(defalias 'list-buffers 'ibuffer)

;; disable visible/visual bell
(setf visible-bell nil)

;; disable suspend
(global-unset-key (kbd "C-z"))

;; while we're at it, make PDF the default LaTeX format
(setq-default TeX-PDF-mode t)

;; 2 spaces for indenting in coffee-mode
(add-hook 'coffee-mode-hook (lambda ()
                              (set (make-local-variable 'tab-width) 2)))

;; bs-show for some more buffer switching
(global-set-key (kbd "C-x B") 'bs-show)

;; eshell settings
(setq eshell-history-size 1024)
(setq eshell-hist-ignoredups t)

;; add "clear" command in eshell
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))))
(add-hook 'eshell-mode-hook (lambda ()
                              (local-set-key (kbd "C-l") (lambda ()
                                                           (interactive)
                                                           (eshell/clear)
                                                           (eshell-interrupt-process)))
                              (local-set-key (kbd "C-c C-l")
                                             'recenter-top-bottom)))

;; fix ansi-term unicode
(add-hook 'term-exec-hook (lambda ()
                            (set-buffer-process-coding-system 'utf-8-unix
                                                              'utf-8-unix)))

;; Flymake with python
(when (load "flymake" t) 
         (defun flymake-pyflakes-init () 
           (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                              'flymake-create-temp-inplace)) 
              (local-file (file-relative-name 
                           temp-file 
                           (file-name-directory buffer-file-name)))) 
             (list "pyflakes" (list local-file)))) 

         (add-to-list 'flymake-allowed-file-name-masks 
                  '("\\.py\\'" flymake-pyflakes-init))) 

   (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Make ' " ( [ and { create their partner
(add-hook 'python-mode-hook
     (lambda ()
      (define-key python-mode-map "\"" 'electric-pair)
      (define-key python-mode-map "\'" 'electric-pair)
      (define-key python-mode-map "(" 'electric-pair)
      (define-key python-mode-map "[" 'electric-pair)
      (define-key python-mode-map "{" 'electric-pair)))
(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;; Auto save files in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")) backup-by-copying t delete-old-versions t kept-new-versions 6 kept-old-versions 2 version-control t)

;; frickin backspace character on a mac
(global-set-key [(control h)] 'delete-backward-char)
(define-key key-translation-map [?\C-h] [?\C-?])
 
;; because I don't understand all of this mac-command business
(setq mac-command-modifier 'alt
       mac-option-modifier 'meta)

;; Says files that end in .tmpl should go to html mode
(add-to-list 'auto-mode-alist '("\\.tmpl$" . html-mode))

(load-theme 'deeper-blue t)

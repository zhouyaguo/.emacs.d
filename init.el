; ------------------------------ set package archives
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")
                         ))

; activate all the packages
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

; ------------------------------ install all packages needed
(setq package-list '(helm 
		     helm-projectile 
		     yasnippet 
		     elpy
		     py-yapf
		     zenburn-theme 
		     projectile 
		     jekyll-modes 
		     markdown-mode 
		     sr-speedbar 
		     projectile-speedbar 
		     company   
		     yaml-mode
		     puppet-mode
		     ))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; ------------------------------ common config

(menu-bar-mode 0)
;(load-theme 'zenburn t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

; ------------------------------ plugins

; company
;(add-hook 'after-init-hook 'global-company-mode)

; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

; ido
(require 'ido)
(ido-mode t)

; sr-speedbar
(require 'sr-speedbar)
;(sr-speedbar-open)

; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)


(require 'projectile-speedbar)
(global-set-key (kbd "<f8>") 'projectile-speedbar-toggle)

; helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)


; pip install jedi flake8 importmagic autopep8 yapf
(elpy-enable)

; markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))                                                                        
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; jekyll
(add-to-list 'auto-mode-alist '("\\.md$" . jekyll-markdown-mode))
;(add-to-list 'auto-mode-alist '("\\.html" . jekyll-html-mode))


; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; py-yapf, used to format python code
(require 'py-yapf)
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

; for mac
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

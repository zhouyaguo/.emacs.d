; ------------------------------ set package archives
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
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
(load-theme 'zenburn t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

; ------------------------------ plugins

; company
(add-hook 'after-init-hook 'global-company-mode)

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

; helm
(require 'helm-config)
(helm-mode 1)


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

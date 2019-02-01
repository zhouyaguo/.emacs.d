; ------------------------------ set package archives
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ("marmalade" . "http://elpa.emacs-china.org/marmalade/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")
                         ))

; activate all the packages
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

; ------------------------------ install all packages needed
(setq package-list '(
             projectile
             helm
             helm-projectile
             ag
             helm-ag
             yasnippet
             yasnippet-snippets
             elpy
             zenburn-theme
             color-theme-sanityinc-tomorrow
             markdown-mode
             company
             yaml-mode
             puppet-mode
             flymake
             flyspell
             neotree
             fill-column-indicator
	     ansible
	     auto-complete
             virtualenvwrapper
             jinja2-mode
             ))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; ------------------------------ common config

(global-set-key (kbd "C-S-K") 'kill-whole-line)

(menu-bar-mode 0)

; theme
;(load-theme 'zenburn t)
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow--define-theme night)

(setq backup-directory-alist '(("." . "~/.emacs.backups")))

; show whitespace
(require 'whitespace)
;(global-whitespace-mode 1)

; 80 column
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-color "darkblue")
(add-hook 'after-change-major-mode-hook 'fci-mode)


(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

; never use tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(setq line-number-mode t)
(setq column-number-mode t)

; ------------------------------ plugins

; company
(add-hook 'after-init-hook 'global-company-mode)

; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

; helm
(load "~/.emacs.d/init-helm")

; projectile
(projectile-mode)

; helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

; pip install jedi flake8 importmagic autopep8 yapf
(elpy-enable)
(setq elpy-rpc-python-command "python2")
(setq elpy-rpc-timeout nil)
(pyvenv-workon "kolla-env")

; virtualenvwrapper
;(require 'virtualenvwrapper)
;(venv-initialize-interactive-shells)
;(venv-initialize-eshell)
;(setq venv-location '("/home/zyg/myvenv/" ))
;(venv-workon "myvenv")

; jinja2-mode
(require 'jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

; markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; py-yapf, used to format python code
;(require 'py-yapf)
;(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

; for mac
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

; flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

; ido
(require 'ido)
(ido-mode t)

; enable line wrapped
(setq-default truncate-lines t)

; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-window-fixed-size nil)

; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/ansible-20170926.1951/dict")
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'ansible)

; ansible
(require 'ansible)
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
(add-hook 'yaml-mode-hook 'auto-complete-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

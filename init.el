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
             ))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; ------------------------------ common config

(menu-bar-mode 0)

; theme
;(load-theme 'zenburn t)
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow--define-theme night)

(setq backup-directory-alist '(("." . "~/.emacs.backups")))

; show whitespace
(require 'whitespace)
;(global-whitespace-mode 1)

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

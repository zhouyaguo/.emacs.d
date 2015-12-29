
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("elpy" . "https://jorgenschaefer.github.io/packages/")
                         ("popkit" . "http://elpa.popkit.org/packages/")))

;; activate all the packages
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; ---------------------------all package i need
(setq package-list '(helm helm-projectile yasnippet elpy auto-complete zenburn-theme projectile jekyll-modes markdown-mode sr-speedbar projectile-speedbar   ))

;; if not installed, then install it
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ---------------------------common config

(menu-bar-mode 0)
(load-theme 'zenburn t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

;; ---------------------------plugins

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; auto-complete
(require 'auto-complete-config)
;; remove ac-source-dictionary
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev  ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)


;; ido
(require 'ido)
(ido-mode t)

;;
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'projectile-speedbar)

;; helm
(require 'helm-config)
;;(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;; jedi: need to 'M-x jedi:install-server' manually!!!!!
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)


;; elpy
;; pip install jedi flake8 importmagic autopep8 yapf
(elpy-enable)

;; markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)                                                          (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))                                                                            
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))                                                                        
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; jekyll
(add-to-list 'auto-mode-alist '("\\.md$" . jekyll-markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.html" . jekyll-html-mode))




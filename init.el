;:; init.el --- Summary

;; inspirations
;; 

;;; Code:
(defconst *spell-check-support-enabled* t)
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; paths
(setenv "PATH"
  (concat (getenv "PATH")
   ":/home/nuxion/.local/bin:/home/nuxion/go/bin:/usr/local/bin:/bin:/usr/bin"
  )
)
;; general settings
(use-package xclip
  :demand t
  :config
  (progn
    (xclip-mode 1)))

;; increase the number of message in the buffer
(setq message-log-max 10000)

;; garbage collection threshold
(setq gc-cons-threshold 100000000
      garbage-collection-messages t)

;; no startup message
(setq inhibit-startup-message t)

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)


;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package`
;; Next, we'll use use-package to configure our packages better,
;; in case you don't know, this package provides a macro to allow
;; you to easily install packages and isolate package configuration
;; in a way that is both performance-oriented and tidy.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; custom packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(neotree ivy-rich counsel go-mode company-lsp company projectile flycheck lsp-ui which-key magit doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; extra packages
;; https://realpython.com/emacs-the-best-python-editor/
;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
;; https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

;;(use-package neotree
;;  :ensure t)
;;(global-set-key [f7] 'neotree-toggle)

(use-package magit
  :ensure t)

;; which-key helps to find keybindings easily.
;; whenever you press a leader key like C-x, a list of all the
;; different keybindings pops up
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;(add-to-list 'load-path "/home/nuxion/emacs.d/evil")
(use-package evil
  :ensure t
)
(require 'evil)
(evil-mode 1)

(package-install 'flycheck)
(global-flycheck-mode)

;; Projectile configuration
(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;;(setq projectile-indexing-method 'native)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Proto/" "~Proyects/covid19" "~/Proyects"))
  (setq projectile-globally-ignored-directories '("node_modules/" "__pycache__/"))
  (setq projectile-globally-ignored-file-suffixes '(".pyc"))
  )




;; language server
;; resources:
; https://scalameta.org/metals/docs/editors/emacs.html
; https://ddavis.io/posts/emacs-python-lsp/
					; https://translate.google.com/translate?hl=en&sl=auto&tl=en&u=https%3A%2F%2Fglassonion.hatenablog.com%2Fentry%2F2019%2F05%2F11%2F134135
; https://wiki.crdb.io/wiki/spaces/CRDB/pages/73105658/Ben+s+Go+Emacs+Setup
(use-package go-mode
  :mode "\\.go$"
  :ensure t
  :commands go-mode
  :config
  (setq gofmt-command "goimports"  )
  ;;(setq tab-width 2 indent-tabs-mode 1)
  ;;(go-eldoc-setup)
  (local-set-key (kbd "M-.") #'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package eglot
  :pin melpa-stable
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  :hook (go-mode . eglot-ensure))

;(use-package lsp-mode
;  :ensure t
;  :commands (lsp lsp-deferred)
;  :hook (
;	 (go-mode . lsp)
;	 (python-mode . lsp)
;	 )
;  )

;(use-package lsp-ui
;  :ensure t
;  :commands lsp-ui-mode
;  :init
; )

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
;; http://oremacs.com/swiper/
(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  )

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))


;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :hook ((emacs-lisp-mode . company-mode)
         (python-mode . company-mode)
         (go-mode . company-mode)
         (typescript-mode . company-mode)
         (restclient-mode . company-mode)
         (js-mode . company-mode)))

(use-package company-lsp
  :ensure t
  :commands company-lsp)



;; Go lang config
;; based on
;; https://arenzana.org/2019/12/emacs-go-mode-revisited/
;(setq lsp-gopls-staticcheck t)
;(setq lsp-eldoc-render-all t)
;(setq lsp-gopls-complete-unimported t)


;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.
;(defun lsp-go-install-save-hooks ()
;  (add-hook 'before-save-hook #'lsp-format-buffer t t)
;  (add-hook 'before-save-hook #'lsp-organize-imports t t))
					;(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;;; init.el ends here

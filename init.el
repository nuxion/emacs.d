;:; init.el --- Summary


;; Package configs
;; If something fails, do
;; package-refresh-contents
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

;; Identtext
(global-set-key (kbd "C-x =") 'indent-according-to-mode)

;; increase the number of message in the buffer
(setq message-log-max 10000)
;; show fullpath in minibuffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq-default frame-title-format "%b (%f)")

;; set default size
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Monospace-11"))

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions) 

;; garbage collection threshold
(setq gc-cons-threshold 100000000
      garbage-collection-messages t)

;; no startup message
(setq inhibit-startup-message t)



;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Let me switch windows with shift-arrows instead of "C-x o" all the time
(windmove-default-keybindings)

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)

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
 '(highlight-indent-guides-method 'bitmap)
 '(package-selected-packages
   '(sphinx-doc python-docstring eglot evil yasnippet highlight-indent-guides highlight-indent-guides-mode yaml-mode eyebrowse eyebrowse-mode git-gutter counsel-etags py-autopep8 all-the-icons company-jedi jedi elpy poetry pyenv-mode pipenv neotree ivy-rich counsel go-mode company-lsp company projectile flycheck lsp-ui which-key magit doom-themes use-package)))
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

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  )

(use-package org
  :ensure t)
;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
 (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(use-package neotree
  :ensure t)
(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
;;(global-set-key [f7] 'neotree-toggle)
(global-set-key [f7] 'neotree-project-dir)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(use-package all-the-icons
  :ensure t)

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
  (setq projectile-project-search-path '("/home/nuxion/Proto/" "/home/nuxion/Proyects/covid19" "/home/nuxion/Proyects"))
  (setq projectile-globally-ignored-directories '("*node_modules" "*__pycache__"))
  (setq projectile-globally-ignored-file-suffixes '("*.pyc"))
  )
  
;(use-package elpy
;  :ensure t
;  :defer t
;  :init
;  (advice-add 'python-mode :before 'elpy-enable))
;(elpy-enable)
;;(setq elpy-rpc-backend "jedi")

(require 'python)
(setq python-shell-interpreter "ipython")

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
  ;;(local-set-key (kbd "M-.") #'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package eglot
  :pin melpa-stable
  :ensure t
  :hook (
         (go-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         )
  )
(setq eglot-server-programs '((go-mode . ("gopls")) (python-mode . ("pyls"))))
;;(setq eglot-server-programs '((python-mode . ("pyls"))))

  ;;:config
  ;;(add-to-list 'eglot-server-programs '((go-mode . ("gopls"))))

;;(add-to-list 'eglot-server-programs '((go-mode . ("gopls"))))
                                        ;;(python-mode . ("pyls")))


;; https://emacs.stackexchange.com/questions/13489/how-do-i-get-emacs-to-recognize-my-python-3-virtual-environment/52673
;; in .dir-locals.el add
;; ((nil . ((pyvenv-activate . "~/repos/my_proj/.venv"))))
;;(use-package pyenv-mode
;;  :ensure t
;;  :config
;;  (pyenv-mode 1))

(use-package pyvenv
  :ensure t
  :init
  )

;(defun my-python-project-dwim-virtualenv ()
;  (interactive)
;  ;; check if .dir-locals.el file already exists and if project-venv-name is in it
;  ;; prompt user to choose existing venv or create a new one
;  ;; update .dir-locals.el file
;  )
;
;(add-hook 'focus-in-hook (lambda ()
;                           (hack-local-variables)
;                           (if (boundp 'project-venv-name)
;                           (progn
;                             (message "Activating %s" project-venv-name)
;                             (pyvenv-workon project-venv-name))
;                           (progn (message "Deactivating")
;                                  (pyvenv-deactivate)))))

; (setq-default mode-line-format (cons '(:exec (concat "venv:" venv-current-name)) mode-line-format))
;;(use-package poetry
;;  :ensure t)
;; alternative
;; https://medium.com/analytics-vidhya/managing-a-python-development-environment-in-emacs-43897fd48c6a
;(use-package pipenv
;  :ensure t
;  :hook (python-mode . pipenv-mode)
;  :init
;  (setq
;   pipenv-projectile-after-switch-function
;   #'pipenv-projectile-after-switch-extended))

(use-package py-autopep8
  :ensure t)

;; load isort package
(load "~/.emacs.d/lisp/py-isort.el")


(defun nux/fix-python()
  "This command run autopep8 and isort."
  (interactive)
  (py-autopep8-buffer)
  (py-isort-buffer)
  )


;; autopep8
;; https://github.com/paetzke/py-autopep8.el
(defun python-mode-keys ()
  "Setting keys for python mode."
  ;;(local-set-key (kbd "C-c C-f") 'py-autopep8-buffer))
  (local-set-key (kbd "C-c C-f") 'nux/fix-python))
(add-hook 'python-mode-hook 'python-mode-keys)


;; docstring completion
(use-package python-docstring
  :ensure t)
(use-package sphinx-doc
  :ensure t)

;; (setq py-set-fill-column-p t)
;;(require-package 'sphinx-doc)
(add-hook 'python-mode-hook (lambda ()
                              (sphinx-doc-mode t)))

;;(require-package 'python-docstring)
(add-hook 'python-mode-hook (lambda ()
                            (python-docstring-mode)))

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
(use-package eyebrowse
  :ensure t
  :config
    (eyebrowse-mode t)
     (setq eyebrowse-new-workspace t)
    )

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


(use-package counsel-etags
  :ensure t
  :bind (("C-}" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))


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

(use-package git-gutter
  :ensure t
  :diminish ""
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:always-show-gutter t)
  (bind-key "C-x v =" 'git-gutter:popup-diff)
  (bind-key "C-x v n" 'git-gutter:next-hunk)
  (bind-key "C-x v p" 'git-gutter:previous-hunk))

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

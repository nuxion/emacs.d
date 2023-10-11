
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

(use-package all-the-icons
  :ensure t
 )

;; matching parenthesis
(show-paren-mode 1)

;;; Code:
(defconst *spell-check-support-enabled* t)
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;;; close brackets
;;; https://emacs.stackexchange.com/questions/28857/how-to-complete-brackets-automatically
(setq electric-pair-preserve-balance nil)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; paths
(setenv "PATH"
  (concat (getenv "PATH")
   ":/home/nuxion/.local/bin:/home/nuxion/go/bin:/usr/local/bin:/bin:/usr/bin"
  )
  )

;; general settings
(use-package xclip
  :ensure t
  :demand t
  :config
  (progn
    (xclip-mode 1)))

;; https://stackoverflow.com/questions/4987760/how-to-change-size-of-split-screen-emacs-windows/4988206
;;(global-set-key (kbd "<C-up>") 'shrink-window)
;;(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

    
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode nil nil (fringe))
 '(fringes-outside-margins t t)
 '(hcl-indent-level 2)
 '(highlight-indent-guides-method 'bitmap)
 '(package-selected-packages
   '(reformatter python-black tide typescript-mode vue-mode jenkinsfile-mode jenkinsfile-mode\.el groovy-mode web-mode prettier-js sbt-mode scala-mode rust-mode docker-compose-mode dockerfile-mode sphinx-doc python-docstring evil yasnippet highlight-indent-guides highlight-indent-guides-mode yaml-mode eyebrowse eyebrowse-mode git-gutter counsel-etags all-the-icons company-jedi jedi elpy poetry neotree ivy-rich counsel go-mode company-lsp company projectile flycheck lsp-ui which-key magit doom-themes use-package))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp))))

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
  (set-frame-parameter frame 'font "Monospace-12"))

;; Fontify current frame ++
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions) 

;; garbage collection threshold
;; 1 gb
;; https://anuragpeshne.github.io/essays/emacsSpeed.html
;; (setq gc-cons-threshold 1000000000
;;      garbage-collection-messages t)

;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun nx-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun nx-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'nx-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'nx-minibuffer-exit-hook)


;; no startup message
(setq inhibit-startup-message t)



;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Let me switch windows with shift-arrows instead of "C-x o" all the time
(windmove-default-keybindings)

;; disable backup
;; (setq backup-inhibited t)
;; disable auto save
;; (setq auto-save-default nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; locking of files for different emacs instances
;; https://stackoverflow.com/questions/5738170/why-does-emacs-create-temporary-symbolic-links-for-modified-files/12974060#12974060
(setq create-lockfiles nil)

;; custom packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

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
;; from https://github.com/jaypei/emacs-neotree/issues/262
;; Set the neo-window-width to the current width of the
  ;; neotree window, to trick neotree into resetting the
  ;; width back to the actual window width.
  ;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
  (eval-after-load "neotree"
    '(add-to-list 'window-size-change-functions
                  (lambda (frame)
                    (let ((neo-window (neo-global--get-window)))
                      (unless (null neo-window)
                        (setq neo-window-width (window-width neo-window)))))))

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
  :config
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
)
(require 'evil)
(evil-mode 1)

(package-install 'flycheck)
(global-flycheck-mode)
;; disable jshint since we prefer eslint checking
;;(setq-default flycheck-disabled-checkers
;;  (append flycheck-disabled-checkers
;;    '(javascript-jshint)))


;; Projectile configuration
(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;;(setq projectile-indexing-method 'native)
  (projectile-mode +1)
  (setq projectile-project-search-path '("/home/nuxion/Proto/" "/home/nuxion/Projects/covid19" "/home/nuxion/Projects" "/home/nuxion/Documents/notes/"))
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

;; look at https://github.com/scalameta/metals/blob/main/docs/editors/emacs.md
;; commented since emacs 29.1 
;; (use-package eglot
;;   :pin melpa-stable
;;   :ensure t
;;   :hook (
;;          (go-mode . eglot-ensure)
;;          (python-mode . eglot-ensure)
;;          (rust-mode . eglot-ensure)
;;          (scala-mode . eglot-ensure)
;;          (js2-mode . eglot-ensure)
;;          (vue-mode . eglot-ensure)
;;          )
;;   )
;; (setq eglot-server-programs '((go-mode . ("gopls")) (vue-mode . ("vls")) (scala-mode . ("metals-emacs")) (rust-mode . ("rls")) (js2-mode . ("javascript-typescript-stdio")) (python-mode . ("pyls"))))
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

;; configuration based on
;; https://blog.fredrikmeyer.net/2020/08/26/emacs-python-venv.html
;; (use-package pyvenv
;;   :ensure t
;;   :init
;;   :config
;;   (pyvenv-mode t)
;; 
;;   ;; Set correct Python interpreter
;;   (setq pyvenv-post-activate-hooks
;;         (list (lambda ()
;;                 (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
;;   (setq pyvenv-post-deactivate-hooks
;;         (list (lambda ()
;;                 (setq python-shell-interpreter "python3")))))

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
; (use-package poetry
;   :ensure t
;   :config
;     (add-hook 'poetry-tracking-mode-hook (lambda () (remove-hook 'post-command-hook 'poetry-track-virtualenv)))
;     (add-hook 'python-mode-hook 'poetry-track-virtualenv)
;     (add-hook 'projectile-after-switch-project-hook 'poetry-track-virtualenv)
;  )
;; alternative
;; https://medium.com/analytics-vidhya/managing-a-python-development-environment-in-emacs-43897fd48c6a
;(use-package pipenv
;  :ensure t
;  :hook (python-mode . pipenv-mode)
;  :init
;  (setq
;   pipenv-projectile-after-switch-function
;   #'pipenv-projectile-after-switch-extended))

;; (use-package python-black
;;   :ensure t
;;   :demand t
;;   :after python
;;   :hook (python-mode . python-black-on-save-mode-enable-dwim))
;; 
;; ;(use-package py-autopep8
;; ;  :ensure t)
;; 
;; ;; load isort package
;; (load "~/.emacs.d/lisp/py-isort.el")
;; 
;; 
;; (defun nux/fix-python()
;;   "This command run autopep8 and isort."
;;   (interactive)
;;   ;(py-autopep8-buffer)
;;   (python-black-buffer)
;;   ;;(py-isort-buffer)
;;   )
;; ;; 
;; ;; 
;; ;; ;; autopep8
;; ;; ;; https://github.com/paetzke/py-autopep8.el
;; (defun python-mode-keys ()
;;   "Setting keys for python mode."
;;   ;;(local-set-key (kbd "C-c C-f") 'py-autopep8-buffer))
;;   (local-set-key (kbd "C-c C-f") 'nux/fix-python))
;; (add-hook 'python-mode-hook 'python-mode-keys)
;; 
;; 
;; ;; docstring completion
;; ;;(use-package python-docstring
;; ;;  :ensure t)
;; ;;(use-package sphinx-doc
;; ;;  :ensure t)
;; 
;; ;; (setq py-set-fill-column-p t)
;; ;;(require-package 'sphinx-doc)
;; ;;(add-hook 'python-mode-hook (lambda ()
;; ;;                              (sphinx-doc-mode t)))
;; 
;; ;;(require-package 'python-docstring)
;; ;;(add-hook 'python-mode-hook (lambda ()
;; ;;                            (python-docstring-mode)))

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
         (python-ts-mode . company-mode)
         (go-mode . company-mode)
         (rust-mode . company-mode)
         (scala-mode . company-mode)
         (typescript-mode . company-mode)
         (restclient-mode . company-mode)
         (js2-mode . company-mode)
         ))

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

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  )

(use-package docker-compose-mode
  :ensure t
  )

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  )
(require 'rust-mode)
;; Go lang config
;; based on
;; https://arenzana.org/2019/12/emacs-go-mode-revisited/
;(setq lsp-gopls-staticcheck t)
;(setq lsp-eldoc-render-all t)
;(setq lsp-gopls-complete-unimported t)

;; Scala setting
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :ensure t
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)
;; javascript
; https://jamiecollinson.com/blog/my-emacs-config/
(setq-default js-indent-level 2)
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :custom
  (js2-mode-show-strict-warnings nil)
  (js2-mode-show-parse-errors nil))
  ;:config
  ;;(setq-default js2-ignored-warnings '("msg.extra.trailing.comma")))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'ghVue)
(flycheck-add-mode 'javascript-eslint 'vue-mode)
(flycheck-add-mode 'javascript-eslint 'ghVue-mode)
;;(flycheck-disabled-checkers 'javascript-jshint)
(setq-default flycheck-disabled-checkers '(javascript-jshint))

;; https://github.com/prettier/prettier-emacs/issues/3
;; Check if vuejs support fail
;;(use-package prettier-js
;;  :ensure t
;;  :config
;;  (add-hook 'js2-mode-hook 'prettier-js-mode)
;;  (add-hook 'web-mode-hook 'prettier-js-mode)
;;)

(use-package web-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  )
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

; also checks https://github.com/munen/emacs.d/#auto-formatting
; from https://gist.github.com/ustun/73321bfcb01a8657e5b8
(defun eslint-fix-file()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

;;(require 'eglot)
;;(require 'web-mode)
;;(define-derived-mode genehack-vue-mode web-mode "ghVue"
;;  "A major mode derived from web-mode, for editing .vue files with LSP support.")
;;(add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))
;;(add-hook 'genehack-vue-mode-hook #'eglot-ensure)
;;(add-to-list 'eglot-server-programs '(genehack-vue-mode "vls"))

;; https://azzamsa.com/n/vue-emacs/
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'eglot-ensure))

;(use-package vue-html-mode
;  :mode "\\.vue\\'"
;)

(require 'mmm-mode)
(use-package mmm-mode
  :ensure t
  :config
  (add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))
  )


(use-package groovy-mode
  :ensure t
  )
(add-hook 'groovy-mode-hook
 (lambda ()
  (c-set-offset 'label 2)))

(use-package jenkinsfile-mode
  :ensure t
 )
(require 'groovy-mode)

(use-package hcl-mode
  :ensure t
  )


;(require 'use-package)
;(use-package tide
;  :ensure t
;  :config
;  (progn
;    (company-mode +1)
;    ;; aligns annotation to the right hand side
;    (setq company-tooltip-align-annotations t)
;    (add-hook 'typescript-mode-hook #'setup-tide-mode)
;    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;  ))

(defun nux/ts-mode-hook ()
 "Set up preferences for typescript mode."
    (tide-setup)

    (local-set-key (kbd "M-j") 'c-indent-new-comment-line)
    (local-set-key (kbd "M-RET") 'c-indent-new-comment-line)

    (flycheck-mode +1)

    (eldoc-mode +1)

    (tide-hl-identifier-mode +1)
    (highlight-symbol-mode -1)

    (company-mode +1)

    (setq typescript-indent-level              2
            typescript-expr-indent-offset        2
            company-tooltip-align-annotations    t

            flycheck-check-syntax-automatically  '(save idle-change mode-enabled)
            flycheck-auto-change-delay           1.5

            whitespace-line-column               120   ;; max line length
            whitespace-style                     '(face lines-tail trailing))
(whitespace-mode))

(require 'use-package)
(use-package typescript-mode
    :ensure t
    :mode (("\\.ts\\'" . typescript-mode))
    :hook
    (typescript-mode . nux/ts-mode-hook))

(use-package tide :ensure t :pin melpa
  :delight
  :commands (tide-setup))


; Open python files in tree-sitter mode.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . python-black-buffer)
              ;;("C-c C-f" . eglot-format-buffer)
              )
  :hook ((python-ts-mode . eglot-ensure)
         ;(python-ts-mode . flyspell-prog-mode)
         ;(python-ts-mode . superword-mode)
         ;(python-ts-mode . hs-minor-mode)
         (python-ts-mode . (lambda () (set-fill-column 88))))
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                             :plugins (
                                       ;:pycodestyle (:enabled :json-false)
                                       :mccabe (:enabled :json-false)
                                       :pyflakes (:enabled :json-false)
                                       :flake8 (:enabled :json-false
                                                :maxLineLength 88)
                                       :ruff (:enabled t
                                              :lineLength 88)
                                       ;:pydocstyle (:enabled t
                                       ;             :convention "numpy")
                                       :yapf (:enabled :json-false)
                                       :autopep8 (:enabled :json-false)
                                       :black (:enabled t
                                               :line_length 88
                                               :cache_config t)))))))




;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.
;(defun lsp-go-install-save-hooks ()
;  (add-hook 'before-save-hook #'lsp-format-buffer t t)
;  (add-hook 'before-save-hook #'lsp-organize-imports t t))
					;(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;;; init.el ends here

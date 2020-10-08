(defun my/insert-line-before ()
  "Inserts a newline(s) above  line containing the cursor"
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (newline)))

;; control shift o insert a new line
(global-set-key (kbd "C-S-o")
		'my/insert-line-before)

(setq eldoc-idle-delay 0)

(defun eldoc-message-now ()
  (interactive))

(defun eldoc--message-command-p (command)
  ;; Should be using advice, but I'm lazy
  ;; One can also loop through `eldoc-message-commands' and empty it out
  (eq command 'eldoc-message-now))

(bind-key "C-c l" #'eldoc-message-now)

(eldoc-add-command 'eldoc-message-now)

(defun my/insert-line-before ()
  "Inserts a newline(s) above  line containing the cursor"
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (newline)))

;; control shift o insert a new line
(global-set-key (kbd "C-S-o")
		'my/insert-line-before)



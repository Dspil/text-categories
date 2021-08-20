;; variables

(defvar typing-category-buffer-prefix "~typing_categories::" "The prefix of typing-category buffers.")
(defvar typing-category nil "Tracks whether typing-category is enabled.")
(defvar typing-category-num 0 "Tracks the current typing-category number.\nCan be a single digit number.")
(defvar typing-category-deleting nil "Tracks whether we are deleting right now.")
(setq-default typing-category nil)
(setq-default typing-category-num 0)
(setq-default typing-categorydeleting nil)

;; helper functions

(defun typing-category-buffer ()
  "Get the helper typing categories buffer name."
  (concat typing-category-buffer-prefix (buffer-name))
  )

(defun inverse-typing-category-buffer ()
  "Return the buffer name which corresonds to the current typing-category buffer."
  (substring-no-properties (format "%s" (current-buffer)) (length typing-category-buffer-prefix))
  )

;; logic

(defun typing-categories-after-changes-fun (beg end len)
  "Do the corresponding change to the typing categories buffer at position (BEG)-(END) with previous length of string (LEN)."
  (unless typing-category-deleting
    (let ((num (+ ?0 typing-category-num)))
      (with-current-buffer (typing-category-buffer)
	(goto-char beg)
	(delete-char len)
	(insert-char num (- end beg))
	)
      )
    )
  )

;; enable-disable

(defun enable-typing-category ()
  "Enable typing-category."
  (setq-local typing-category t)
  (setq-local typing-category-num 0)
  (let ((helper (typing-category-buffer))
	(characters (- (point-max) (point-min))))
    (generate-new-buffer helper)
    (with-current-buffer (get-buffer helper)
      (insert-char ?0 characters))
    )
  (add-hook 'after-change-functions 'typing-categories-after-changes-fun t t)
  (message "Typing categories enabled")
  )

(defun disable-typing-category ()
  "Disable typing-category."
  (setq-local typing-category nil)
  (let ((helper (typing-category-buffer)))
    (kill-buffer helper)
    )
  (remove-hook 'after-change-functions 'typing-categories-after-changes-fun t)
  (message "Typing categories disabled")
  )

;; commands

(defun typing-category ()
  "Toggle typing-category."
  (interactive)
  (if typing-category (disable-typing-category) (enable-typing-category))
  )

(defun change-typing-category (CATEGORY)
  "Change the typing-category of the typing-category package.\n(CATEGORY): the mode to change to."
  (interactive "NEnter category (0-9): ")
  (unless typing-category (enable-typing-category))
  (setq-local typing-category-num CATEGORY)
  )

(defun delete-typing-category (CATEGORY)
  "Delete each character belonging to typing mode (CATEGORY)."
  (interactive "NEnter category to delete(0-9): ")
  (setq CATEGORY (+ ?0 CATEGORY))
  (setq-local typing-category-deleting t)
  (save-excursion
    (with-current-buffer (typing-category-buffer)
      (goto-char (point-min))
      (while (not (eq (point) (point-max)))
	(if (eq (char-after) CATEGORY)
	    (let ((point (point)))
	      (delete-char 1)
	      (with-current-buffer (inverse-typing-category-buffer)
		(goto-char point)
		(delete-char 1)
		)
	      )
	  (forward-char)
	  )
	)
      )
    )
  (setq-local typing-category-deleting nil)
  )

;; key bindings !!!!!! temporary

(global-set-key (kbd "C-x t m") 'change-typing-category)
(global-set-key (kbd "C-x t t") 'typing-category)
(global-set-key (kbd "C-x t d") 'delete-typing-category)

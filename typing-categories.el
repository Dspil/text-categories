;;; typing-categories.el --- Typing in different categories to allow editing of seamingly unrelated file positions                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

;; variables

(defvar typing-category-buffer-prefix "~typing_categories::" "The prefix of typing-category buffers.")
(defvar typing-category nil "Tracks whether typing-category is enabled.")
(defvar typing-category-num 0 "Tracks the current typing-category number.\nCan be a single digit number.")
(defvar typing-category-priv-indent nil "Tracks the indentations.")
(setq-default typing-category nil)
(setq-default typing-category-num 0)


;; helper functions

(defun typing-category-buffer ()
  "Get the helper typing categories buffer name."
  (concat typing-category-buffer-prefix (buffer-name))
  )

(defun inverse-typing-category-buffer ()
  "Return the buffer name which corresonds to the current typing-category buffer."
  (substring-no-properties (format "%s" (current-buffer)) (length typing-category-buffer-prefix))
  )

(defun is-whitespace (c)
  "Check if the character C given is whitespace."
  (or (eq c ?\n) (eq c ?\t) (eq c ?\v) (eq c ?\s))
  )

(defun typing-category-delete-char ()
  "Delete the same character in the typing categories buffer."
  (unless (eq (point) (point-max))
    (let ((point (point)))
      (with-current-buffer (typing-category-buffer)
	(goto-char point)
	(delete-char 1)
	)
      )
    )
  )

(defun typing-category-delete-backward-char ()
  "Delete the same character in the typing categories buffer."
  (unless (eq (point) (point-min))
    (let ((point (- (point) 1)))
      (with-current-buffer (typing-category-buffer)
	(goto-char point)
	(delete-char 1)
	)
      )
    )
  )

(defun typing-category-hungry-delete-forward ()
  "Hungry delete the same characters in the typing categories buffer."
  (unless (eq (point) (point-max))
    (save-excursion
      (let ((point (point))
	    (chars 0))
	(if (is-whitespace (char-after))
	    (while (is-whitespace (char-after))
	      (setq chars (1+ chars))
	      (goto-char (1+ (point)))
	      )
	  (setq chars 1)
	  )
	(with-current-buffer (typing-category-buffer)
	  (goto-char point)
	  (delete-char chars)
	  )
	)
      )
    )
  )

(defun typing-category-hungry-delete-backward ()
  "Hungry delete the same characters in the typing categories buffer."
  (unless (eq (point) (point-min))
    (save-excursion
      (let ((point (point))
	    (chars 0))
	(if (is-whitespace (char-before))
	    (while (is-whitespace (char-before))
	      (setq chars (1+ chars))
	      (goto-char (1- (point))))
	  (setq chars 1)
	  )
	(with-current-buffer (typing-category-buffer)
	  (goto-char (- point chars))
	  (delete-char chars)
	  )
	)
      )
    )
  )

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
  (message "Typing categories enabled")
  )

(defun disable-typing-category ()
  "Disable typing-category."
  (setq-local typing-category nil)
  (let ((helper (typing-category-buffer)))
    (kill-buffer helper)
    )
  (message "Typing categories disabled")
  )

;; functions to be hooked

(defun typing-category-post-self-insert ()
  "Insert the category number in the helper buffer."
  (when typing-category
    (dotimes (count (or current-prefix-arg 1))
      (let ((num typing-category-num)
	    (point (- (point) 1)))
	(with-current-buffer (typing-category-buffer)
	  (goto-char point)
	  (insert-char (+ num ?0))
	  )
	)
      )
    )
  )

(defun typing-category-count-indentation ()
  (save-excursion
    (let ((count 0))
      (beginning-of-line)
      (while (is-whitespace (char-after))
	(setq count (1+ count))
	(forward-char)
	)
      count
      )
    )
  )

(defun typing-category-pre-command ()
  "Pre command hook to track for character differences."
  (when typing-category
    (message "%s" this-command) ;for debugging
    (when (eq this-command 'hungry-delete-forward)
      (typing-category-hungry-delete-forward)
      )
    (when (eq this-command 'hungry-delete-backward)
      (typing-category-hungry-delete-backward)
      )
    (when (eq this-command 'delete-char)
      (typing-category-delete-char)
      )
    (when (eq this-command 'backward-delete-char-untabify)
      (typing-category-backward-delete-char-untabify)
      )
    (when (eq this-command 'delete-backward-char)
      (typing-category-delete-backward-char)
      )
    (when (string-match "indent" (format "%s" this-command))
      (setq-local typing-category-priv-indent  (typing-category-count-indentation))
      )
    )
  )

(defun typing-category-post-command ()
  "Post command hook to track for character differences."
  (when typing-category
    (when (string-match "indent" (format "%s" this-command))
      (message (format "POST COMMAND %s" this-command))
      (save-excursion
	(let ((countafter (typing-category-count-indentation))
	      (countbefore typing-category-priv-indent))
	  (beginning-of-line)
	  (let ((point (point)))
	    (with-current-buffer (typing-category-buffer)
	      (goto-char point)
	      (when (> countbefore countafter)
		(delete-char (- countbefore countafter))
		)
	      (when (< countbefore countafter)
		(insert-char (+ typing-category-num ?0) (- countafter countbefore))
		)
	      )
	    )
	  )
	)
      )
    )
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
  (save-excursion
    (with-current-buffer (typing-category-buffer)
      (goto-char 1)
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
  )

;; hooks

(add-hook 'post-self-insert-hook 'typing-category-post-self-insert)
(add-hook 'pre-command-hook 'typing-category-pre-command)
(add-hook 'post-command-hook 'typing-category-post-command)

;; key bindings !!!!!! temporary

(global-set-key (kbd "C-x t m") 'change-typing-category)
(global-set-key (kbd "C-x t t") 'typing-category)
(global-set-key (kbd "C-x t d") 'delete-typing-category)


;(provide 'typing-modes)

;;; typing-categories.el ends here

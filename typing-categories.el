;;; typing-categories.el --- Typing categories for characters -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/typing-categories
;; Package-Requires: ((emacs "24.3"))

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

;; Annotates each character in the buffer with a category chosen by
;; the user.  The user can delete all characters belonging to a
;; certain category.

;;; Code:

;; variables

(defvar-local typing-categories-buffer-prefix "~typing_categories::" "The prefix of typing-categories buffers.")
(defvar-local typing-categories nil "Tracks whether typing-categories is enabled.")
(defvar-local typing-categories-num 0 "Tracks the current typing category number.\nCan be a single digit number.")
(defvar-local typing-categories-deleting nil "Tracks whether we are deleting right now.")
(defvar-local typing-categories-default 0 "Holds the default typing category number.")

;; helper functions

(defun typing-categories-buffer ()
  "Get the helper typing categories buffer name."
  (concat typing-categories-buffer-prefix (buffer-name)))

(defun typing-categories-inverse-buffer ()
  "Return the buffer name which corresonds to the current typing-categories buffer."
  (substring-no-properties (format "%s" (current-buffer)) (length typing-categories-buffer-prefix)))

;; logic

(defun typing-categories-after-changes-fun (beg end len)
  "Do the corresponding change to the typing categories buffer at position (BEG)-(END) with previous length of string (LEN).  This function is hooked to 'after-change-functions'."
  (unless typing-categories-deleting
    (let ((num (+ ?0 typing-categories-num)))
      (with-current-buffer (typing-categories-buffer)
	(goto-char beg)
	(delete-char len)
	(insert-char num (- end beg))))))

(defun typing-categories-after-find-file ()
  "Check if the file opened has a corresponding typing-categories file and load it."
  (let ((typing-categories-file
	 (concat
	  (file-name-directory (buffer-file-name))
	  (concat typing-categories-buffer-prefix (file-name-nondirectory (buffer-file-name))))))
    (when
	(file-exists-p
	 typing-categories-file)
      (find-file-noselect typing-categories-file)
      (typing-categories-enable t))))

(defun typing-categories-after-save-file ()
  "Check if typing-categories is active when saving a file and save the typing-categories corresponding buffer."
  (when typing-categories
    (let ((typing-categories-file
	   (concat
	    (file-name-directory (buffer-file-name))
	    (concat typing-categories-buffer-prefix (file-name-nondirectory (buffer-file-name))))))
      (with-current-buffer (typing-categories-buffer)
	(write-file typing-categories-file)))))

(defun typing-categories-after-kill-buffer ()
  "Check if typing-categories is active when killing a buffer to kill the typing-categories corresponding buffer."
  (when typing-categories
    (kill-buffer (typing-categories-buffer))))

;; enable-disable

(defun typing-categories-enable (&optional nocreate)
  "Enable typing-categories.  When (NOCREATE) is non nil a new buffer is not generated as one exists in the file system."
  (setq-local typing-categories t)
  (unless nocreate
    (setq-local typing-categories-num typing-categories-default)
    (let ((char (+ typing-categories-num ?0))
	  (helper (typing-categories-buffer))
	  (characters (- (point-max) (point-min))))
      (generate-new-buffer helper)
      (with-current-buffer (get-buffer helper)
	(insert-char char characters))))
  (typing-categories-enable-hooks)
  (message "Typing categories enabled"))

(defun typing-categories-disable ()
  "Disable typing-categories."
  (setq-local typing-categories nil)
  (when (buffer-file-name)
    (let ((typing-categories-file
	   (concat
	    (file-name-directory (buffer-file-name))
	    (concat typing-categories-buffer-prefix (file-name-nondirectory (buffer-file-name))))))
      (when (file-exists-p typing-categories-file)
	(delete-file typing-categories-file))))
  (let ((helper (typing-categories-buffer)))
    (kill-buffer helper))
  (typing-categories-disable-hooks)
  (message "Typing categories disabled"))

;; commands

(defun typing-categories ()
  "Toggle typing-categories."
  (interactive)
  (if typing-categories (typing-categories-disable) (typing-categories-enable)))

(defun typing-categories-change (category)
  "Change the typing category of the typing-categories package.\n(CATEGORY): the category to change to."
  (interactive "NEnter category (0-9): ")
  (unless typing-categories (typing-categories-enable))
  (setq-local typing-categories-num category))

(defun typing-categories-delete (category)
  "Delete each character belonging to typing category (CATEGORY)."
  (interactive "NEnter category to delete(0-9): ")
  (when typing-categories
    (setq category (+ ?0 category))
    (setq-local typing-categories-deleting t)
    (save-excursion
      (with-current-buffer (typing-categories-buffer)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (eq (char-after) category)
	      (let ((point (point)))
		(delete-char 1)
		(with-current-buffer (typing-categories-inverse-buffer)
		  (goto-char point)
		  (delete-char 1)))
	    (forward-char)))))
    (setq-local typing-categories-deleting nil))
  (when (not typing-categories) (message "Typing categories are not active.")))

(defun typing-categories-current ()
  "Report the current typing category."
  (interactive)
  (message "Current typing category: %d" typing-categories-num))

(defun typing-categories-reset ()
  "Reset the typing category to the default one."
  (interactive)
  (setq-local typing-categories-num typing-categories-default)
  (typing-categories-current))

;; hooks

(defun typing-categories-enable-hooks ()
  "Add typing categories hooks."
  (add-hook 'after-change-functions 'typing-categories-after-changes-fun t t)
  (add-hook 'find-file-hook 'typing-categories-after-find-file)
  (add-hook 'after-save-hook 'typing-categories-after-save-file)
  (add-hook 'kill-buffer-hook 'typing-categories-after-kill-buffer))

(defun typing-categories-disable-hooks ()
  "Remove all typing categories hooks."
  (remove-hook 'after-change-functions 'typing-categories-after-changes-fun t)
  (remove-hook 'find-file-hook 'typing-categories-after-find-file)
  (remove-hook 'after-save-hook 'typing-categories-after-save-file)
  (remove-hook 'kill-buffer-hook 'typing-categories-after-kill-buffer))

(provide 'typing-categories)

;;; typing-categories.el ends here

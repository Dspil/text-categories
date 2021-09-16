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

;; While writing anything you can choose a category for which your next changes
;; will belong to.  Default category is 0 and categories can be a single digit number.
;; Any changes that belong to a certain category can be erased using delete-typing-category.

;;; Code:

;; variables

(defvar typing-category-buffer-prefix "~typing_categories::" "The prefix of typing-categories buffers.")
(defvar typing-category nil "Tracks whether typing-categories is enabled.")
(defvar typing-category-num 0 "Tracks the current typing category number.\nCan be a single digit number.")
(defvar typing-category-deleting nil "Tracks whether we are deleting right now.")
(defvar typing-category-default 0 "Holds the default typing category number.")
(setq-default typing-category nil)
(setq-default typing-category-num 0)
(setq-default typing-category-deleting nil)
(setq-default typing-category-default 0)

;; helper functions

(defun typing-category-buffer ()
  "Get the helper typing categories buffer name."
  (concat typing-category-buffer-prefix (buffer-name))
  )

(defun inverse-typing-category-buffer ()
  "Return the buffer name which corresonds to the current typing-categories buffer."
  (substring-no-properties (format "%s" (current-buffer)) (length typing-category-buffer-prefix))
  )

;; logic

(defun typing-categories-after-changes-fun (beg end len)
  "Do the corresponding change to the typing categories buffer at position (BEG)-(END) with previous length of string (LEN).  This function is hooked to 'after-change-functions'."
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

(defun typing-category-after-find-file ()
  "Check if the file opened has a corresponding typing-categories file and load it."
  (let ((typing-category-file
	 (concat
	  (file-name-directory (buffer-file-name))
	  (concat typing-category-buffer-prefix (file-name-nondirectory (buffer-file-name)))
	  )))
    (when
	(file-exists-p
	 typing-category-file
	 )
      (find-file-noselect typing-category-file)
      (enable-typing-category t)
      )
    )
  )

(defun typing-category-after-save-file ()
  "Check if typing-categories is active when saving a file and save the typing-categories corresponding buffer."
  (when typing-category
    (let ((typing-category-file
	   (concat
	    (file-name-directory (buffer-file-name))
	    (concat typing-category-buffer-prefix (file-name-nondirectory (buffer-file-name)))
	    )))
      (with-current-buffer (typing-category-buffer)
	(write-file typing-category-file)
	)
      )
    )
  )

(defun typing-category-after-kill-buffer ()
  "Check if typing-categories is active when killing a buffer to kill the typing-categories corresponding buffer."
  (when typing-category
    (kill-buffer (typing-category-buffer))
    )
  )

;; enable-disable

(defun enable-typing-category (&optional nocreate)
  "Enable typing-categories.  When (NOCREATE) is non nil a new buffer is not generated as one exists in the file system."
  (setq-local typing-category t)
  (unless nocreate
    (setq-local typing-category-num typing-category-default)
    (let ((char (+ typing-category-num ?0))
	  (helper (typing-category-buffer))
	  (characters (- (point-max) (point-min))))
      (generate-new-buffer helper)
      (with-current-buffer (get-buffer helper)
	(insert-char char characters))
      )
    )
  (add-hook 'after-change-functions 'typing-categories-after-changes-fun t t)
  (message "Typing categories enabled")
  )

(defun disable-typing-category ()
  "Disable typing-categories."
  (setq-local typing-category nil)
  (when (buffer-file-name)
    (let ((typing-category-file
	   (concat
	    (file-name-directory (buffer-file-name))
	    (concat typing-category-buffer-prefix (file-name-nondirectory (buffer-file-name)))
	    )))
      (when (file-exists-p typing-category-file)
	(delete-file typing-category-file)
	)
      )
    )
  (let ((helper (typing-category-buffer)))
    (kill-buffer helper)
    )
  (remove-hook 'after-change-functions 'typing-categories-after-changes-fun t)
  (message "Typing categories disabled")
  )

;; commands

(defun typing-category ()
  "Toggle typing-categories."
  (interactive)
  (if typing-category (disable-typing-category) (enable-typing-category))
  )

(defun change-typing-category (CATEGORY)
  "Change the typing category of the typing-categories package.\n(CATEGORY): the category to change to."
  (interactive "NEnter category (0-9): ")
  (unless typing-category (enable-typing-category))
  (setq-local typing-category-num CATEGORY)
  )

(defun delete-typing-category (CATEGORY)
  "Delete each character belonging to typing category (CATEGORY)."
  (interactive "NEnter category to delete(0-9): ")
  (when typing-category
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
  (when (not typing-category) (message "Typing categories are not active."))
  )

(defun current-typing-category ()
  "Report the current typing category."
  (interactive)
  (message "Current typing category: %d" typing-category-num)
  )

(defun reset-typing-category ()
  "Reset the typing category to the default one."
  (interactive)
  (setq-local typing-category-num typing-category-default)
  (current-typing-category)
  )

;; hooks

(add-hook 'find-file-hook 'typing-category-after-find-file)
(add-hook 'after-save-hook 'typing-category-after-save-file)
(add-hook 'kill-buffer-hook 'typing-category-after-kill-buffer)

(provide 'typing-categories)

;;; typing-categories.el ends here

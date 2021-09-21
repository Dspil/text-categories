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

(defvar-local typing-categories nil "Tracks whether typing-categories is enabled.")
(defvar-local typing-categories-category nil "Tracks the current typing category number.\nCan be a single digit number.")
(defvar-local typing-categories-suppress-changes nil "Tracks whether we are deleting right now.")
(defvar-local typing-categories-default "0" "Holds the default typing category number.")
(defvar-local typing-categories-file-prefix "~typing_categories::" "The prefix of typing categories files.")
(defvar-local typing-categories-viz-prefix "~typing_categories_viz::" "The prefix of typing categories vizualization buffer.")

(defvar typing-categories-save t "If t, saving the buffer saves the typing categories of its characters.")
(defvar typing-categories-colorwheel '("dark orange" "deep pink" "chartreuse" "deep sky blue" "yellow" "orchid" "spring green" "sienna1") "Contains the colors of the categories to show in the visualization.")

;; logic

(defun typing-categories-viz-buffer ()
  "Return a visualization buffer name corresponding to the current buffer."
  (concat typing-categories-viz-prefix (buffer-name)))

(defun typing-categories-filename ()
  "Return a filename corresponding to the current buffer."
  (concat typing-categories-file-prefix (buffer-name)))

(defun typing-categories-dump (data filename)
  "Dump DATA in the file FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun typing-categories-load (filename)
  "Restore data from the file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(defun typing-categories-list ()
  "Return the distinct typing categories existing in the current buffer."
  (let ((found '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((property (get-text-property (point) 'typing-categories-category)))
	  (unless (member property found)
	    (setq found (cons property found))))
	(forward-char)))
    found))

(defun typing-categories-load-categories ()
  "Load categories from file if it exists."
  (if (file-exists-p (typing-categories-filename))
      (progn
	(setq-local typing-categories-suppress-changes t)
	(let* ((data (typing-categories-load (typing-categories-filename)))
	       (foundmap (car data))
	       (catstring (cdr data)))
	  (save-excursion
	    (goto-char (point-min))
	    (while (not (eobp))
	      (put-text-property (point) (1+ (point)) 'typing-categories-category (car (rassoc (string-to-char (substring catstring (1- (point)) (point))) foundmap)))
	      (forward-char))))
	(setq-local typing-categories-suppress-changes nil))
    (put-text-property (point-min) (point-max) 'typing-categories-category typing-categories-default)))

(defun typing-categories-color-map-helper (found colors acc)
  "Helper function for making the color map using FOUND categories COLORS as the available colors and ACC for tail recursion."
  (if (equal (length found) 0)
      acc
    (when (equal (length colors) 0)
      (setq colors typing-categories-colorwheel))
    (typing-categories-color-map-helper (cdr found) (cdr colors) (cons (cons (car found) (car colors)) acc))))

(defun typing-categories-color-map (found)
  "Make an association list of FOUND categories with cycling the colorwheel."
  (typing-categories-color-map-helper found '() '()))

(defun typing-categories-make-legend (found)
  "Print the description of the visualization buffer for FOUND categories."
  (let ((str "Helper Buffer for inspecting typing categories.\nCategories are:\n")
	(nostart nil))
    (while (> (length found) 0)
      (setq str (concat str (and nostart " | ") (propertize (car found) 'typing-categories-category (car found))))
      (setq nostart t)
      (setq found (cdr found)))
    (insert (concat str "\nBuffer contents:\n\n"))))

;; enable-disable

(defun typing-categories-enable ()
  "Enable typing categories."
  (setq-local typing-categories t)
  (setq-local typing-categories-category typing-categories-default)
  (if typing-categories-save
      (typing-categories-load-categories)
    (put-text-property (point-min) (point-max) 'typing-categories-category typing-categories-default))
  (typing-categories-enable-hooks)
  (message "Typing categories enabled"))

(defun typing-categories-disable ()
  "Disable typing categories."
  (setq-local typing-categories nil)
  (typing-categories-disable-hooks)
  (when (buffer-file-name)
    (let ((typing-categories-file
	   (concat
	    (file-name-directory (buffer-file-name))
	    (concat typing-categories-file-prefix (file-name-nondirectory (buffer-file-name))))))
      (when (file-exists-p typing-categories-file)
	(delete-file typing-categories-file))))
  (message "Typing categories disabled"))

;; commands

(defun typing-categories ()
  "Toggle typing categories."
  (interactive)
  (if typing-categories (typing-categories-disable) (typing-categories-enable)))

(defun typing-categories-change (category)
  "Change the typing category.\n CATEGORY: the category to change to."
  (interactive "sEnter category: ")
  (unless typing-categories (typing-categories-enable))
  (setq-local typing-categories-category category))

(defun typing-categories-delete (category)
  "Delete each character belonging to typing category CATEGORY."
  (interactive "sEnter category to delete: ")
  (when typing-categories
    (setq-local typing-categories-suppress-changes t)
    (goto-char (point-min))
    (while (not (eobp))
      (if (equal (get-text-property (point) 'typing-categories-category) category)
	  (delete-char 1)
	(forward-char)))
    (setq-local typing-categories-suppress-changes nil))
  (when (not typing-categories) (message "Typing categories are not active.")))

(defun typing-categories-report ()
  "Report the current typing category and all the categories that exist in the buffer."
  (interactive)
  (message "Current typing category: %s\nCategories in buffer: %s" typing-categories-category (typing-categories-list)))

(defun typing-categories-reset ()
  "Reset the typing category to the default one."
  (interactive)
  (setq-local typing-categories-category typing-categories-default)
  (typing-categories-report))

(defun typing-categories-visualize ()
  "Show the typing categories of characters in a separate buffer."
  (interactive)
  (let* ((name (buffer-name))
	 (found (typing-categories-list))
	 (cmap (typing-categories-color-map found)))
    (with-current-buffer (get-buffer-create (typing-categories-viz-buffer))
      (erase-buffer)
      (typing-categories-make-legend found)
      (insert-buffer-substring name)
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((cat (get-text-property (point) 'typing-categories-category))
	       (color (cdr (assoc cat cmap))))
	  (put-text-property (point) (1+ (point)) 'face (list :foreground color)))
	(forward-char))
      (goto-char (point-min))
      (setq-local buffer-read-only t))
    (pop-to-buffer (typing-categories-viz-buffer))))

(defun typing-categories-enable-on-find-file ()
  "If ENABLE is t, when loading a file that has a corresponding typing categories file, it will enable the typing categories and load them from the file."
  (interactive)
  (add-hook 'find-file-hook 'typing-categories-after-load-fun))

;; hooks

(defun typing-categories-after-load-fun ()
  "After opening a file, if a categories file exist, enable typing categories."
  (when (file-exists-p (typing-categories-filename))
    (typing-categories-enable)))

(defun typing-categories-after-changes-fun (beg end _)
  "Do the corresponding change to the typing categories buffer at position (BEG)-(END) with previous length of string (LEN).  This function is hooked to 'after-change-functions'."
  (unless typing-categories-suppress-changes
    (put-text-property beg end 'typing-categories-category typing-categories-category)))

(defun typing-categories-after-save-fun ()
  "Save the typing categories of characters of the saved buffer in a file for later use."
  (when (and typing-categories (> (point-max) 1))
    (let* ((found (typing-categories-list))
	   (foundmap (mapcar* #'cons found (number-sequence 1 (length found))))
	   (catstring ""))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (setq catstring (concat catstring (string (cdr (assoc (get-text-property (point) 'typing-categories-category) foundmap)))))
	  (forward-char)))
      (typing-categories-dump (cons foundmap catstring) (typing-categories-filename)))))

(defun typing-categories-enable-hooks ()
  "Add typing categories hooks."
  (add-hook 'after-change-functions 'typing-categories-after-changes-fun t t)
  (when typing-categories-save
    (add-hook 'after-save-hook 'typing-categories-after-save-fun)))

(defun typing-categories-disable-hooks ()
  "Remove all typing categories hooks."
  (remove-hook 'after-change-functions 'typing-categories-after-changes-fun t)
  (remove-hook 'after-save-hook 'typing-categories-after-save-fun))

(provide 'typing-categories)

;;; typing-categories.el ends here

;;; text-categories.el --- Assign text categories to a buffer for mass deletion -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos <dennisspiliopoylos@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/text-categories
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

;; libraries

(require 'cl-lib)

;; variables

(defvar-local text-categories nil "Tracks whether text-categories is enabled.")
(defvar-local text-categories-category nil "Tracks the current text category number.\nCan be a single digit number.")
(defvar-local text-categories-suppress-changes nil "Tracks whether we are deleting right now.")
(defvar-local text-categories-default "0" "Holds the default text category number.")
(defvar-local text-categories-file-prefix "~text_categories::" "The prefix of text categories files.")
(defvar-local text-categories-viz-prefix "~text_categories_viz::" "The prefix of text categories vizualization buffer.")

(defvar text-categories-save t "If t, saving the buffer saves the text categories of its characters.")
(defvar text-categories-colorwheel '("dark orange" "deep pink" "chartreuse" "deep sky blue" "yellow" "orchid" "spring green" "sienna1") "Contains the colors of the categories to show in the visualization.")

;; logic

(defun text-categories-viz-buffer ()
  "Return a visualization buffer name corresponding to the current buffer."
  (concat text-categories-viz-prefix (buffer-name)))

(defun text-categories-kill-viz ()
  "Kill the visualization buffer if it exists."
  (when (get-buffer (text-categories-viz-buffer))
    (kill-buffer (text-categories-viz-buffer))))

(defun text-categories-filename ()
  "Return a filename corresponding to the current buffer."
  (concat text-categories-file-prefix (buffer-name)))

(defun text-categories-dump (data filename)
  "Dump DATA in the file FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun text-categories-load (filename)
  "Restore data from the file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (bobp))
    (read (current-buffer))))

(defun text-categories-list ()
  "Return the distinct text categories existing in the current buffer."
  (let ((found '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((property (get-text-property (point) 'text-categories-category)))
	  (unless (member property found)
	    (setq found (cons property found))))
	(forward-char)))
    found))

(defun text-categories-load-categories ()
  "Load categories from file if it exists."
  (if (file-exists-p (text-categories-filename))
      (progn
	(setq-local text-categories-suppress-changes t)
	(let* ((data (text-categories-load (text-categories-filename)))
	       (foundmap (car data))
	       (catstring (cdr data)))
	  (save-excursion
	    (goto-char (point-min))
	    (while (not (eobp))
	      (put-text-property (point) (1+ (point)) 'text-categories-category (car (rassoc (string-to-char (substring catstring (1- (point)) (point))) foundmap)))
	      (forward-char))))
	(setq-local text-categories-suppress-changes nil))
    (put-text-property (point-min) (point-max) 'text-categories-category text-categories-default)))

(defun text-categories-color-map-helper (found colors acc)
  "Helper function for making the color map using FOUND categories COLORS as the available colors and ACC for aggregating it."
  (if (equal (length found) 0)
      acc
    (when (equal (length colors) 0)
      (setq colors text-categories-colorwheel))
    (text-categories-color-map-helper (cdr found) (cdr colors) (cons (cons (car found) (car colors)) acc))))

(defun text-categories-color-map (found)
  "Make an association list of FOUND categories with cycling the colorwheel."
  (text-categories-color-map-helper found '() '()))

(defun text-categories-make-legend (found)
  "Print the description of the visualization buffer for FOUND categories."
  (let ((str "Helper Buffer for inspecting text categories.\nCategories are:\n")
	(nostart nil))
    (while (> (length found) 0)
      (setq str (concat str (and nostart " | ") (propertize (car found) 'text-categories-category (car found))))
      (setq nostart t)
      (setq found (cdr found)))
    (insert  str "\nBuffer contents:\n\n")))

;; enable-disable

(defun text-categories-enable ()
  "Enable text categories."
  (setq-local text-categories t)
  (setq-local text-categories-category text-categories-default)
  (if text-categories-save
      (text-categories-load-categories)
    (put-text-property (point-min) (point-max) 'text-categories-category text-categories-default))
  (text-categories-enable-hooks)
  (message "Text categories enabled"))

(defun text-categories-disable ()
  "Disable text categories."
  (setq-local text-categories nil)
  (text-categories-disable-hooks)
  (text-categories-kill-viz)
  (when (buffer-file-name)
    (let ((text-categories-file
	   (concat
	    (file-name-directory (buffer-file-name))
	    (concat text-categories-file-prefix (file-name-nondirectory (buffer-file-name))))))
      (when (file-exists-p text-categories-file)
	(delete-file text-categories-file))))
  (message "Text categories disabled"))

;; commands

(defun text-categories ()
  "Toggle text categories."
  (interactive)
  (if text-categories (text-categories-disable) (text-categories-enable)))

(defun text-categories-change (category)
  "Change the text category.\n CATEGORY: the category to change to."
  (interactive "sEnter category: ")
  (unless text-categories (text-categories-enable))
  (setq-local text-categories-category category))

(defun text-categories-delete (category)
  "Delete each character belonging to text category CATEGORY."
  (interactive "sEnter category to delete: ")
  (when text-categories
    (setq-local text-categories-suppress-changes t)
    (let ((prevpoint (point)))
      (goto-char (point-min))
      (while (not (eobp))
	(if (equal (get-text-property (point) 'text-categories-category) category)
	    (progn
	      (delete-char 1)
	      (when (<= (point) prevpoint)
		(setq prevpoint (1- prevpoint))))
	  (forward-char)))
      (goto-char prevpoint)
      (setq-local text-categories-suppress-changes nil)))
  (when (not text-categories) (message "Text categories are not active.")))

(defun text-categories-report ()
  "Report the current text category and all the categories that exist in the buffer."
  (interactive)
  (when text-categories
    (message "Current text category: %s\nCategories in buffer: %s" text-categories-category (text-categories-list)))
  (when (not text-categories) (message "Text categories are not active.")))

(defun text-categories-reset ()
  "Reset the text category to the default one."
  (interactive)
  (setq-local text-categories-category text-categories-default)
  (text-categories-report))

(defun text-categories-visualize ()
  "Show the text categories of characters in a separate buffer."
  (interactive)
  (when text-categories
    (let* ((name (buffer-name))
	   (found (text-categories-list))
	   (cmap (text-categories-color-map found)))
      (with-current-buffer (get-buffer-create (text-categories-viz-buffer))
	(setq-local buffer-read-only nil)
	(erase-buffer)
	(text-categories-make-legend found)
	(insert-buffer-substring name)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((cat (get-text-property (point) 'text-categories-category))
		 (color (cdr (assoc cat cmap))))
	    (put-text-property (point) (1+ (point)) 'face (list :foreground color)))
	  (forward-char))
	(goto-char (point-min))
	(setq-local buffer-read-only t))
      (pop-to-buffer (text-categories-viz-buffer))))
  (when (not text-categories) (message "Text categories are not active.")))

(defun text-categories-enable-on-find-file ()
  "If ENABLE is t, when loading a file that has a corresponding text categories file, it will enable the text categories and load them from the file."
  (interactive)
  (add-hook 'find-file-hook 'text-categories-after-load-fun))

;; hooks

(defun text-categories-after-load-fun ()
  "After opening a file, if a categories file exist, enable text categories."
  (when (file-exists-p (text-categories-filename))
    (text-categories-enable)))

(defun text-categories-after-changes-fun (beg end _)
  "Do the corresponding change to the text categories buffer at position (BEG)-(END) with previous length of string (LEN).  This function is hooked to 'after-change-functions'."
  (unless text-categories-suppress-changes
    (put-text-property beg end 'text-categories-category text-categories-category)))

(defun text-categories-after-save-fun ()
  "Save the text categories of characters of the saved buffer in a file for later use."
  (when (and text-categories (> (point-max) 1))
    (let* ((found (text-categories-list))
	   (foundmap (cl-mapcar #'cons found (number-sequence 1 (length found))))
	   (catstring ""))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (setq catstring (concat catstring (string (cdr (assoc (get-text-property (point) 'text-categories-category) foundmap)))))
	  (forward-char)))
      (text-categories-dump (cons foundmap catstring) (text-categories-filename)))))

(defun text-categories-enable-hooks ()
  "Add text categories hooks."
  (add-hook 'after-change-functions 'text-categories-after-changes-fun t t)
  (add-hook 'kill-buffer-hook 'text-categories-kill-viz)
  (when text-categories-save
    (add-hook 'after-save-hook 'text-categories-after-save-fun)))

(defun text-categories-disable-hooks ()
  "Remove all text categories hooks."
  (remove-hook 'after-change-functions 'text-categories-after-changes-fun t)
  (remove-hook 'kill-buffer-hook 'text-categories-kill-viz)
  (remove-hook 'after-save-hook 'text-categories-after-save-fun))

(provide 'text-categories)

;;; text-categories.el ends here
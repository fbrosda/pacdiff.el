;;; pacdiff.el --- Manage pacnew files -*- lexical-binding: t -*-

;; Copyright (C) 2022 Fabian Brosda

;; Author: Fabian Brosda <fabi3141@gmx.de>
;; URL: https://github.com/fbrosda/pacdiff.el
;; Package-Requires: ((emacs "28.1"))
;; Version: 1.0

;;; Commentary:

;; This package provides the function 'pacdiff' to manage *.pacnew and
;; *.pacsave files, which are created by the Arch Linux package
;; manager.

;;; Code:
(require 'subr-x)

(defgroup pacdiff nil
  "Manage pacdiff files from within Emacs."
  :group 'packages)

(defcustom pacdiff-buffer "*pacdiff*"
  "The name of the pacdiff buffer."
  :type 'string
  :group 'pacdiff)

(defcustom pacdiff-cmd "/usr/bin/pacdiff"
  "The binary used to create pacdiff output."
  :type 'string
  :group 'pacdiff)

;; Untested, but this way someone who prefers doas over sudo could
;; change this, right?
(defcustom pacdiff-tramp "/sudo::"
  "Tramp Method to get write permissions for the config files."
  :type 'string
  :group 'pacdiff)

(defface pacdiff-pacnew-face
  '((t :foreground "forest green"))
  "Face to display pacnew files."
  :group 'pacdiff)

(defface pacdiff-pacsave-face
  '((t :foreground "blue"))
  "Face to display pacsave files."
  :group 'pacdiff)

(defface pacdiff-other-face
  '((t :foreground "red"))
  "Face to highlight other files.

This should normally not be used as only pacenew and pacsave
files should be listed."
  :group 'pacdiff)

(defconst pacdiff--line-marker "> ")

(defun pacdiff--find ()
  "Find packages files needing a merge using pacdiff."
  (process-lines pacdiff-cmd "-o"))

(defun pacdiff--pacnew-p (file)
  "Check if FILE is a pacnew file."
  (string-match-p "\\.pacnew$" file))

(defun pacdiff--pacsave-p (file)
  "Check if FILE is a pacsave file."
  (string-match-p "\\.pacsave$" file))

(defun pacdiff--get-face (file)
  "Get fontcolor depending on type of FILE."
  (cond ((pacdiff--pacnew-p file) 'pacdiff-pacnew-face)
        ((pacdiff--pacsave-p file) 'pacdiff-pacsave-face)
        (t 'pacdiff-other-face)))

(defun pacdiff--get-file ()
  "Get the filename from an entry line in the pacdiff buffer."
  (let* ((l1 (thing-at-point 'line t))
         (l2 (string-chop-newline l1))
         (l3 (string-trim-left l2 pacdiff--line-marker)))
    (replace-regexp-in-string ":.*$" "" l3)))

(defun pacdiff--get-base (filename)
  "Get the base filename from the given FILENAME.

This removes the .pacnew/.pacsave ending from FILENAME."
  (file-name-sans-extension filename))

(defun pacdiff-next-button ()
  "Jump to next button."
  (interactive)
  (forward-button 1 t t t))

(defun pacdiff-previous-button ()
  "Jump to previous button."
  (interactive)
  (backward-button 1 t t t))

(defun pacdiff-next (&optional cnt)
  "Move to next entry including optional count CNT."
  (interactive "^P")
  (cond
   ((eq cnt '-)
    (pacdiff-previous nil))
   ((< (prefix-numeric-value cnt) 0)
    (pacdiff-previous (- (prefix-numeric-value cnt))))
   (t
    (forward-char 1)
    (re-search-forward pacdiff--line-marker nil t (prefix-numeric-value cnt))
    (beginning-of-line))))

(defun pacdiff-previous (&optional cnt)
  "Move to previous entry including optional count via CNT."
  (interactive "^P")
  (cond
   ((eq cnt '-)
    (pacdiff-next nil))
   ((< (prefix-numeric-value cnt) 0)
    (pacdiff-next (- (prefix-numeric-value cnt))))
   (t
    (re-search-backward pacdiff--line-marker nil t (prefix-numeric-value cnt)))))

(defun pacdiff-edit (&optional _)
  "Edit original file and pacnew/pacsave via ediff."
  (interactive)
  (let* ((filename (pacdiff--get-file))
         (basename (pacdiff--get-base filename)))
    (unless (and basename (or (pacdiff--pacnew-p filename)
                              (pacdiff--pacsave-p filename)))
      (error "Could not determine original file from pacdiff output"))
    (ediff (concat pacdiff-tramp filename) (concat pacdiff-tramp basename))))

(defun pacdiff--update-line (r)
  "Replace the first character in line with R."
  (let ((inhibit-read-only t))
    (save-excursion
     (beginning-of-line)
     (delete-char 1)
     (insert r))))

(defun pacdiff-remove (&optional _)
  "Remove the pacnew/pacsave file."
  (interactive)
  (let ((filename (pacdiff--get-file)))
    (when (y-or-n-p (format "Delete file: \"%s\"?" filename))
      (delete-file (concat pacdiff-tramp filename))
      (pacdiff--update-line "-")
      (pacdiff-next))))

(defun pacdiff-overwrite (&optional _)
  "Overwrite original file with pacnew/pacsave."
  (interactive)
  (let* ((filename (pacdiff--get-file))
         (basename (pacdiff--get-base filename)))
    (unless (and basename (or (pacdiff--pacnew-p filename)
                              (pacdiff--pacsave-p filename)))
      (error "Could not determine original file from pacdiff output"))
    (when (y-or-n-p (format "Move \"%s\" to \"%s\"?" filename basename))
      (rename-file (concat pacdiff-tramp filename) basename t)
      (pacdiff--update-line "+")
      (pacdiff-next))))

(defun pacdiff--format-files (files)
  "Create pacdiff entry for each file in FILES.

This inserts first the filename and then the different buttons to
handle each entry."
  (dolist (f files)
    (insert pacdiff--line-marker)
    (insert (propertize f 'face (pacdiff--get-face f)))
    (insert ": ")
    (insert-text-button "(e)dit"
                        'action #'pacdiff-edit
                        'face 'bold
                        'help-echo "Edit files with ediff")
    (insert " ")
    (insert-text-button "(r)emove"
                        'action #'pacdiff-remove
                        'face 'bold
                        'help-echo "Remove pacnew/pacsave")
    (insert " ")
    (insert-text-button "(o)verwrite"
                        'action #'pacdiff-overwrite
                        'face 'bold
                        'help-echo "Overwrite with pacnew/pacsave")
    (insert "\n")))


(defun pacdiff--revert-buffer (&rest _)
  "Refresh the pacdiff buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (when (equal (current-buffer) (get-buffer pacdiff-buffer))
     (erase-buffer)
     (pacdiff--setup (current-buffer))
     (goto-char (point-min)))))

(defun pacdiff--setup (buffer)
  "Setup pacdiff BUFFER."
  (with-current-buffer buffer
    (setq-local revert-buffer-function #'pacdiff--revert-buffer)
    (insert (format "-*- results \"%s\" -*-\n\n\n" pacdiff-cmd))
    (let ((files (pacdiff--find)))
      (if (length= files 0)
          (insert "no files need merging.")
        (insert (format "%d file(s):\n" (length files)))
        (pacdiff--format-files files)))))

(defvar pacdiff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap scroll-up-command] #'pacdiff-next)
    (define-key map (kbd "n") #'pacdiff-next)
    (define-key map [remap scroll-down-command] #'pacdiff-previous)
    (define-key map (kbd "p") #'pacdiff-previous)
    (define-key map (kbd "e") #'pacdiff-edit)
    (define-key map (kbd "r") #'pacdiff-remove)
    (define-key map (kbd "o") #'pacdiff-overwrite)
    (define-key map (kbd "<tab>") #'pacdiff-next-button)
    (define-key map (kbd "<backtab>") #'pacdiff-previous-button)
    map)
  "Keymap for Pacdiff mode.")

(define-derived-mode pacdiff-mode special-mode "Pacdiff"
  "Major mode to edit pacdiff files.

\\{pacdiff-mode-map}")

;;;###autoload
(defun pacdiff ()
  "Open Buffer showing pacdiff files."
  (interactive)
  (with-current-buffer (get-buffer-create pacdiff-buffer)
    (unless (derived-mode-p 'pacdiff-mode)
      (pacdiff--setup (current-buffer))
      (goto-char (point-min))
      (pacdiff-mode))
    (pop-to-buffer (current-buffer)
                   '((display-buffer-reuse-window display-buffer-same-window)))))

(provide 'pacdiff)

;;; pacdiff.el ends here.

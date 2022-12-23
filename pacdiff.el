;;; pacdiff.el --- Manage pacnew files -*- lexical-binding: t -*-

;; Copyright (C) 2022 Fabian Brosda

;; Author: Fabian Brosda <fabi3141@gmx.de>
;; URL: https://github.com/fbrosda/pacdiff.el
;; Version: 1.0
;; Package-Requires: ((emacs "28.0"))
;; Keywords: pacman, pacdiff

;;; Commentary:

;; This package provides the function `pacdiff' to manage *.pacnew and
;; *.pacsave files, which are created by the Arch Linux package
;; manager.

;;; Code:

(defcustom pacdiff-buffer "*pacdiff*"
  "The name of the pacdiff buffer.")

(defcustom pacdiff-cmd "/usr/bin/pacdiff -o"
  "The binary used to create pacdiff output.")

;; Untested, but this way someone who prefers doas over sudo could
;; change this, right?
(defcustom pacdiff-tramp "/sudo::"
  "Tramp Method to get write permissions for the config files.")

(defun pacdiff--find ()
  "Find packages files needing a merge using pacdiff."
  (let ((files (shell-command-to-string pacdiff-cmd)))
    (seq-filter (lambda (x) (length> x 0)) (split-string files "\n"))))

(defun pacdiff--pacnew? (file)
  (string-match-p "\\.pacnew$" file))

(defun pacdiff--pacsave? (file)
  (string-match-p "\\.pacsave$" file))

(defun pacdiff--get-color (file)
  (cond ((pacdiff--pacnew? file) "forest green")
	((pacdiff--pacsave? file) "blue")
	(t "red")))

(defun pacdiff--get-file ()
  "Get the filename from an entry line in the pacdiff buffer."
  (let* ((l1 (thing-at-point 'line t))
	 (l2 (string-chop-newline l1))
	 (l3 (string-trim-left l2 "* ")))
    (replace-regexp-in-string ":.*$" "" l3)))

(defun pacdiff--get-base (filename)
  "Get the base filename from a FILENAME including the
.pacnew/.pacsave ending."
  (replace-regexp-in-string "\\.pac\\(new\\|save\\)$" "" filename))

(defun pacdiff-next-button ()
  "Jump to next button."
  (interactive)
  (let* ((pos (point))
	 (btn (next-button pos)))
    (if (eq btn nil)
	(goto-char (next-button (point-min)))
      (goto-char btn))))

(defun pacdiff-previous-button ()
  "Jump to previous button."
  (interactive)
  (let* ((pos (point))
	 (btn (previous-button pos)))
    (if (eq btn nil)
	(goto-char (previous-button (point-max)))
      (goto-char btn))))

(defun pacdiff-next ()
  "Move to next entry."
  (interactive)
  (forward-char 1)
  (re-search-forward "^*" nil t)
  (beginning-of-line))

(defun pacdiff-previous ()
  "Move to previous entry."
  (interactive)
  (re-search-backward "^*" nil t))

(defun pacdiff-edit (&optional button)
  "Edit original file and pacnew/pacsave via ediff."
  (interactive)
  (let* ((filename (pacdiff--get-file))
	 (basename (pacdiff--get-base filename)))
    (unless (and basename (or (pacdiff--pacnew? filename)
			      (pacdiff--pacsave? filename)))
      (error "Could not determine original file from pacdiff output."))
    (ediff (concat pacdiff-tramp filename) (concat pacdiff-tramp basename))))

(defun pacdiff-remove (&optional button)
  "Remove the pacnew/pacsave file."
  (interactive)
  (let ((filename (pacdiff--get-file)))
    (when (y-or-n-p (format "Delete file: \"%s\"" filename))
      (delete-file (concat pacdiff-tramp filename)))))

(defun pacdiff-overwrite (&optional button)
  "Overwrite original file with pacnew/pacsave."
  (interactive)
  (let* ((filename (pacdiff--get-file))
	 (basename (pacdiff--get-base filename)))
    (unless (and basename (or (pacdiff--pacnew? filename)
			      (pacdiff--pacsave? filename)))
      (error "Could not determine original file from pacdiff output."))
    (when (y-or-n-p (format "Move \"%s\" to \"%s\"" filename basename))
      (rename-file (concat pacdiff-tramp filename) basename t))))

(defun pacdiff--format-files (files)
  "Insert one line for each file in FILES into the pacdiff buffer,
including the different edit buttons."
  (dolist (f files)
    (insert "* ")
    (insert (propertize f 'face `(:foreground ,(pacdiff--get-color f))))
    (insert ": ")
    (insert-text-button "(e)dit"
			'action 'pacdiff-edit
			'face 'bold
			'help-echo "Edit files with ediff")
    (insert " ")
    (insert-text-button "(r)emove"
			'action 'pacdiff-remove
			'face 'bold
			'help-echo "Remove pacnew")
    (insert " ")
    (insert-text-button "(o)verwrite"
			'action 'pacdiff-overwrite
			'face 'bold
			'help-echo "Overwrite with pacnew")
    (insert "\n")))

(defun pacdiff--setup (buffer)
  "Setup pacdiff BUFFER."
  (with-current-buffer buffer
    (insert (format "-*- results \"%s\" -*-\n\n\n" pacdiff-cmd))
    (let ((files (pacdiff--find)))
      (if (length= files 0)
	  (insert "no files need merging.")
	(insert (format "%d file(s):\n" (length files)))
	(pacdiff--format-files files)))))

(defun pacdiff-revert-buffer ()
  "Refresh the pacdiff buffer."
  (interactive)
  (when (equal (current-buffer) (get-buffer pacdiff-buffer))
    (read-only-mode -1)
    (erase-buffer)
    (pacdiff--setup (current-buffer))
    (read-only-mode)
    (goto-line 1)))

(defun pacdiff-quit-window ()
  "Kill buffer and quit the pacdiff session."
  (interactive)
  (quit-window t))

(defvar pacdiff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'pacdiff-next)
    (define-key map (kbd "p") 'pacdiff-previous)
    (define-key map [remap quit-window] 'pacdiff-quit-window)
    (define-key map [remap revert-buffer] 'pacdiff-revert-buffer)
    (define-key map (kbd "e") 'pacdiff-edit)
    (define-key map (kbd "r") 'pacdiff-remove)
    (define-key map (kbd "o") 'pacdiff-overwrite)
    (define-key map (kbd "<tab>") 'pacdiff-next-button)
    (define-key map (kbd "<backtab>") 'pacdiff-previous-button)
    map)
  "Keymap for Pacdiff mode.")

(define-derived-mode pacdiff-mode special-mode "Pacdiff"
  "Major mode to edit pacdiff files.

\\{pacdiff-mode-map}")

;;;###autoload
(defun pacdiff ()
  "Open Buffer showing pacdiff files."
  (interactive)
  (let* ((buffer (get-buffer pacdiff-buffer)))
    (unless buffer
      (setq buffer (generate-new-buffer pacdiff-buffer))
      (pacdiff--setup buffer))
    (switch-to-buffer-other-window buffer)
    (pacdiff-mode)
    (goto-line 1)))

(provide 'pacdiff)

;;; pacdiff.el ends here.

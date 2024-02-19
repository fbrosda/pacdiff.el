;;; pacdiff.el --- Manage pacdiff files -*- lexical-binding: t -*-

;; Copyright (C) 2024 Fabian Brosda

;; Author: Fabian Brosda <fabi3141@gmx.de>
;;         Nicholas Vollmer
;; URL: https://github.com/fbrosda/pacdiff.el
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: BSD-3-Clause
;; Version: 0.1

;;; Commentary:

;; This package provides an Arch Linux 'pacdiff' file management UI
;; via the `pacdiff' command.

;;; Code:

(defgroup pacdiff nil
  "Major mode for pacdiff file management."
  :group 'packages)

(defface pacdiff-pacnew '((t :foreground "forest green"))
  "Face for pacnew files.")
(defface pacdiff-pacsave '((t :foreground "blue"))
  "Face for pacsave files.")

(defcustom pacdiff-cmd "/usr/bin/pacdiff"
  "The binary used to create pacdiff output."
  :type 'string)

;; Untested, but this way someone who prefers doas over sudo could change this.
(defcustom pacdiff-tramp "/sudo::"
  "Tramp Method to get write permissions for the config files."
  :type 'string)

(defconst pacdiff--regexp "\\.pac\\(save\\|new\\)\\'"
  "Regular expression to match recognized pacdiff files.")

(defun pacdiff--files ()
  "Find packages files needing a merge using pacdiff."
  (delq nil (mapcar (lambda (name) (and (string-match-p pacdiff--regexp name) name))
                    (process-lines pacdiff-cmd "-o"))))

;;Compatibility for Emacs 28.1's `button-buttonize'
(defalias 'pacdiff-button (if (fboundp 'buttonize) 'buttonize 'button-buttonize))

(defun pacdiff--entry (filename &optional padding)
  "Return pacdiff entry string for FILENAME.
If PADDING is non-nil, use it to pad space between file name and buttons."
  (propertize
   (format
    (format "%%-%ds %%s %%s %%s" (or padding 0))
    (propertize
     filename 'face (intern-soft (format "pacdiff-%s" (file-name-extension filename))))
    (pacdiff-button "(e)dit" #'pacdiff-edit "Edit files with ediff")
    (pacdiff-button "(r)emove" #'pacdiff-remove "Remove pacnew/pacsave")
    (pacdiff-button "(o)verwrite" #'pacdiff-overwrite "Overwrite with pacnew/pacsave"))
   'pacdiff filename))

(defun pacdiff--draw (&rest _)
  "Draw pacdiff buffer."
  (unless (derived-mode-p 'pacdiff-mode) (error "Buffer not in `pacdiff-mode'"))
  (let* ((inhibit-read-only t)
         (files (pacdiff--files))
         (line (line-number-at-pos))
         (col (current-column))
         (max (let ((max 0))
                (dolist (f files max) (setq max (max (length f) max))))))
    (erase-buffer)
    (setq header-line-format (format "%S (%d files)" pacdiff-cmd (length files)))
    (insert (if (zerop (length files))
                "No files found."
              (mapconcat (lambda (f) (pacdiff--entry f max)) files "\n")))
    (goto-char (point-min))
    (unless (zerop (length files))
      (forward-line (1- line))
      (forward-char col))))

(defun pacdiff--current-filename ()
  "Return pacdiff filename associated with point.
Signal an error if no file is found."
  (or (get-text-property (point) 'pacdiff) (user-error "No pacdiff file at point")))

(defun pacdiff-remove (&optional _)
  "Remove the pacnew/pacsave file."
  (interactive nil pacdiff-mode)
  (let ((filename (pacdiff--current-filename)))
    (when (yes-or-no-p (format "Delete file: \"%s\"?" filename))
      (delete-file (concat pacdiff-tramp filename))
      (pacdiff--draw))))

(defun pacdiff-overwrite (&optional _)
  "Overwrite original file with pacnew/pacsave."
  (interactive nil pacdiff-mode)
  (let* ((filename (pacdiff--current-filename))
         (noext (file-name-sans-extension filename)))
    (when (yes-or-no-p (format "Move \"%s\" to \"%s\"?" filename noext))
      (rename-file (concat pacdiff-tramp filename) noext t)
      (pacdiff--draw))))

;;@MAYBE: redraw pacdiff buffer on `ediff-quit-hook'?
(defun pacdiff-edit (&optional _)
  "Edit original file and pacnew/pacsave via ediff."
  (interactive nil pacdiff-mode)
  (let* ((filename (pacdiff--current-filename))
         (noext (file-name-sans-extension filename)))
    (ediff (concat pacdiff-tramp filename) (concat pacdiff-tramp noext))))

(defvar pacdiff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "e") #'pacdiff-edit)
    (define-key map (kbd "r") #'pacdiff-remove)
    (define-key map (kbd "o") #'pacdiff-overwrite)
    map)
  "Keymap for Pacdiff mode.")

(define-derived-mode pacdiff-mode special-mode "Pacdiff"
  "Major mode to edit pacdiff files.

\\{pacdiff-mode-map}"
  (setq-local revert-buffer-function #'pacdiff--draw)
  (hl-line-mode)
  (button-mode))

;;;###autoload
(defun pacdiff ()
  "Open Buffer showing pacdiff files."
  (interactive)
  (with-current-buffer (get-buffer-create "*pacdiff*")
    (unless (derived-mode-p 'pacdiff-mode)
      (pacdiff-mode)
      (pacdiff--draw))
    (pop-to-buffer (current-buffer)
                   '((display-buffer-reuse-window display-buffer-same-window)))))

(provide 'pacdiff)

;;; pacdiff.el ends here.

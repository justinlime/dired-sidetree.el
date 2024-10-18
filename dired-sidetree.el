;;; dired-sidetree.el --- A sidetree, powered by dired -*- lexical-binding:t -*-
;;
;; Author: justinlime
;; URL: https://github.com/justinlime/dired-sidetree.el
;; Version: 1.0
;; Keywords: frames tools
;; Package-Requires: ((emacs "25.1"))
;;
;;; License
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;; A very simple sidetree, powered by Dired
;;
;;; Code:

(defgroup dired-sidetree nil
  "A sidetree, powered by Dired."
  :prefix "dired-sidetree-"
  :group 'files)

(defcustom dired-sidetree-size 30
  "Size of the sidetree."
  :type 'fixnum
  :group 'dired-sidetree)

(defcustom dired-sidetree-side 'left
  "Side of the sidetree."
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom))
  :group 'dired-sidetree)

(defvar dired-sidetree-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'dired-sidetree-open)
    (define-key map (kbd "<return>") #'dired-sidetree-open)
    (define-key map (kbd "C-m") #'dired-sidetree-open)
    (define-key map "^" #'dired-sidetree-open-up) map)
  "Keymap for `dired-sidetree'.")

(defvar dired-sidetree--buffers nil
  "Alist of the sidetree buffers.")

(defun dired-sidetree--unadvertise (dir)
  "Remove a directory from Dired's visibility.
Argument DIR Directory to unadvertise."
  (let* ((dir* (expand-file-name dir))
         (dir** (directory-file-name dir*))
         (dir*** (concat dir** "/")))
    (dolist (d `(,dir ,dir* ,dir** ,dir***))
      (dired-unadvertise d))))

(defun dired-sidetree--get-window ()
  "Get the window belonging to the sidetree."
  (car (delq nil (mapcar
                   (lambda (sidetree)
                     (let ((window (get-buffer-window (cdr sidetree))))
                       (when (window-live-p window) window)))
                  dired-sidetree--buffers))))

(defun dired-sidetree--get-buffer (dir)
  "Get the buffer present in the sidetree window.
Argument DIR Directory to match to a buffer."
  (car (delq nil (mapcar
                   (lambda (sidetree)
                     ;; remove stale entries
                     (unless (buffer-live-p (cdr sidetree))
                       (delq sidetree dired-sidetree--buffers))
                     (when (and (string= (car sidetree) (directory-file-name (expand-file-name dir)))
                                (buffer-live-p (cdr sidetree)))
                       (cdr sidetree)))
                  dired-sidetree--buffers))))

(defun dired-sidetree--display-buffer (buffer)
  "Display a given BUFFER in the sidetree window."
  (display-buffer-in-side-window buffer
    `((slot . 0)
      (side . ,dired-sidetree-side)
      (window-width . ,dired-sidetree-size)
      (preserve-size . (t . nil))))
  (select-window (get-buffer-window buffer)))

(defun dired-sidetree--spawn-buffer (dir)
  "Create a new sidetree buffer.
Argument DIR Directory used to spawn the buffer."
  ;; Force dired to spawn a fresh buffer
  (dired-sidetree--unadvertise dir)
  (let* ((dir (directory-file-name (expand-file-name dir)))
         (buffer (dired-noselect dir)))
    (with-current-buffer buffer
      (rename-buffer (format " %s-sidetree" (buffer-name)))
      (dired-hide-details-mode t)
      (use-local-map dired-sidetree-map))
    (add-to-list 'dired-sidetree--buffers `(,dir . ,buffer))
    ;; Make sure dired doesnt try to control the new buffer
    (dired-sidetree--unadvertise dir)
    buffer))

(defun dired-sidetree-toggle ()
  "Toggle the sidetree."
  (interactive)
  (let ((window (dired-sidetree--get-window)))
    (if window
        (delete-window window)
        (let ((buffer (dired-sidetree--get-buffer default-directory)))
          (if buffer
              (dired-sidetree--display-buffer buffer)
              (dired-sidetree--display-buffer (dired-sidetree--spawn-buffer default-directory)))))))

(defun dired-sidetree-open (&optional dir)
  "Open a given DIR with sidetree, or the file under the cursor."
  (interactive)
  (let* ((file (or dir (dired-get-file-for-visit)))
         (buffer (dired-sidetree--get-buffer file)))
    (if buffer
        (dired-sidetree--display-buffer buffer)
        (if (file-directory-p file)
          (dired-sidetree--display-buffer (dired-sidetree--spawn-buffer file))
          ;; FIXME probably a better way to do this
          (let ((other-windows (delete nil (mapcar (lambda (win) (unless (eq (selected-window) win) win)) (window-list)))))
            (select-window (car other-windows))
            (find-file file))))))

(defun dired-sidetree-open-up ()
  "Helper function, to move up one directory."
  (interactive)
  (dired-sidetree-open ".."))

(provide 'dired-sidetree)
;;; dired-sidetree.el ends here.

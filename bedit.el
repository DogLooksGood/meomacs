;; -*- lexical-binding: t; -*-
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; BEdit - Batched Edit
;;
;; This package provides facilities to help editing multiple palaces at once with Emacs' kmacro.
;;
;; Recommended configuration:
;;
;; (require 'bedit)
;;
;; (define-key global-map (kbd "C-M-g") bedit-prefix-map)
;;
;; Usages:
;;
;; 1. (Optional) Set editing scope with secondary selection.
;;
;; There are several ways to create a secondary selection.
;;
;;     - Drag the text with mouse left button while holding Meta key.
;;     - Enable `bedit-extending-mode'   C-M-g C-M-g
;;     - Convert from current region     C-M-g SPC
;;     - Select current defun            C-M-g d
;;     - Select current line             C-M-g l
;;     - Select current paragraph        C-M-g p
;;
;; With `bedit-extending-mode', move the cursor in arbitrary way to extend the secondary selection.
;; Press C-M-g C-M-g again to bring the cursor back to the original position.
;;
;; When no secondary selection, the following edit applies to the whole buffer.
;;
;; 2. Specify the pattern for the palaces to edit.
;;
;; Currently the supported patters are:
;;
;;     - Position in current column      C-M-g o
;;     - Chars like the one at point     C-M-g c
;;     - Symbols like the one at point   C-M-g .
;;     - Same content with region        C-M-g s
;;
;; These command will bring you to a temporary buffer where you record the kmacros.
;;
;; 3. Record the kmacro.
;;
;; The kmacro recording should already be activated.
;;
;; Now record the macro. Once record is done, the macro will apply according to the scope and
;; pattern.
;;
;; Cancellation:
;;
;; To cancel the editing, press C-g at any step or press C-M-g C-g to cancel secondary selection.
;;; Code:

(defvar bedit--editing-search nil
  "The type of current editing thing.")

(defvar bedit--editing-search-offset nil
  "The offset between after-search position and the original position.")

(defvar bedit--editing-post-macro nil
  "The command to run after each macro iteration.")

(defvar bedit--editing-buffer nil
  "The buffer name we are editing.")

(defvar bedit--extending-start nil
  "Where we entered `bedit-extending-mode'.")

(defcustom bedit-inhibit-modes '(font-lock-mode company-mode)
  "Disable these modes during execution.")

(defvar bedit-prefix-map
  (let ((m (make-keymap)))
    (define-key m (kbd "SPC")   #'bedit-region-to-secondary-selection)
    (define-key m (kbd "C-g")   #'bedit-quit)
    (define-key m (kbd "C-M-g") #'bedit-extending-mode)
    (define-key m (kbd "d")     #'bedit-defun-to-secondary-selection)
    (define-key m (kbd "p")     #'bedit-paragraph-to-secondary-selection)
    (define-key m (kbd "l")     #'bedit-line-to-secondary-selection)
    (define-key m (kbd ".")     #'bedit-start-with-symbols-like-this)
    (define-key m (kbd "s")     #'bedit-start-with-search-like-this)
    (define-key m (kbd "o")     #'bedit-start-with-lines)
    (define-key m (kbd "e")     #'bedit-start-with-end-of-lines)
    (define-key m (kbd "c")     #'bedit-start-with-chars-like-this)
    m)
  "The keymap for commands to use with BEdit")

(defun bedit--extend-secondary-selection ()
  (let ((beg (overlay-start mouse-secondary-overlay))
        (end (overlay-end mouse-secondary-overlay)))
    (move-overlay mouse-secondary-overlay
                  (min beg (point))
                  (max end (point)))))

(defvar bedit-recording-mode-map
  (let ((m (make-keymap)))
    (define-key m [remap kmacro-end-or-call-macro] #'bedit--finish-recording)
    (define-key m [remap kmacro-end-macro]         #'bedit--finish-recording)
    (define-key m [remap keyboard-quit]            #'bedit--cancel-recording)
    m))

(defvar bedit-extending-mode-map
  (let ((m (make-keymap)))
    (define-key m [remap bedit-extending-mode] #'bedit-extending-reset-position)
    (define-key m [remap keyboard-quit]        #'bedit-extending-quit)
    m))

(defun bedit-extending-reset-position ()
  (interactive)
  (when bedit--extending-start
    (goto-char (marker-position bedit--extending-start))))

(defun bedit-extending-quit ()
  (interactive)
  (delete-overlay mouse-secondary-overlay)
  (bedit-extending-mode -1))

(define-minor-mode bedit-extending-mode
  "The internal mode used when extending a secondary selection."
  :inti-value nil
  :lighter " BEdit[Ext]"
  :keymap bedit-extending-mode-map
  (if bedit-extending-mode
      (progn
        (move-overlay mouse-secondary-overlay (point) (point))
        (setq bedit--extending-start (make-marker))
        (set-marker bedit--extending-start (point))
        (add-hook 'post-command-hook 'bedit--extend-secondary-selection 0 t))

    (remove-hook 'post-command-hook 'bedit--extend-secondary-selection t)
    (when bedit--extending-start
      (set-marker bedit--extending-start nil)
      (setq bedit--extending-start nil))))

(define-minor-mode bedit-recording-mode
  "The internal mode used when recording a kmacro."
  :init-value nil
  :lighter " BEdit[Rec]"
  :keymap bedit-recording-mode-map)

(defmacro bedit--wrap-collapse-undo (&rest body)
  "Like `progn' but perform BODY with undo collapsed."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
        (success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
           ;; Don't truncate any undo data in the middle of this.
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           (,success nil))
       (unwind-protect
           (progn
             (activate-change-group ,handle)
             (prog1 ,(macroexp-progn body)
               (setq ,success t)))
         (if ,success
             (progn
               (accept-change-group ,handle)
               (undo-amalgamate-change-group ,handle))
           (cancel-change-group ,handle))))))

(defun bedit-region-to-secondary-selection ()
  "Convert current region to secondary selection."
  (interactive)
  (secondary-selection-from-region)
  (deactivate-mark t))

(defun bedit--thing-to-secondary-selection (thing)
  "Put current THING into secondary selection."
  (save-mark-and-excursion
    (let* ((beg (beginning-of-thing thing))
           (end (end-of-thing thing)))
      (when (region-active-p)
        (delete-overlay mouse-secondary-overlay))
      (move-overlay mouse-secondary-overlay beg end))))

(defun bedit-defun-to-secondary-selection ()
  "Put current defun into secondary selection."
  (interactive)
  (bedit--thing-to-secondary-selection 'defun))

(defun bedit-paragraph-to-secondary-selection ()
  "Put current paragraph into secondary selection."
  (interactive)
  (bedit--thing-to-secondary-selection 'paragraph))

(defun bedit-line-to-secondary-selection ()
  "Put current line into secondary selection."
  (interactive)
  (bedit--thing-to-secondary-selection 'line))

(defun bedit--create-recording-buffer ()
  (let* ((mm major-mode)
         (bufname (format " *bedit on %s*" (buffer-name)))
         (orig-buf (current-buffer))
         (p (point)))
    (bedit-extending-mode -1)
    ;; Kill the old existing buffer first
    (when-let* ((buf (get-buffer bufname)))
      (kill-buffer buf))
    (let ((buf (get-buffer-create bufname)))
      (switch-to-buffer buf)
      (funcall mm)
      (insert-buffer orig-buf)
      (setq bedit--editing-buffer (buffer-name orig-buf))
      (goto-char p)
      (bedit-recording-mode 1)
      buf)))

(defmacro bedit--bedit--require-secondary-selection (&rest body)
  `(if (not (overlayp mouse-secondary-overlay))
       (message "Secondary selection is required")
     ,@body))

(defun bedit--start-macro ()
  (call-interactively #'kmacro-start-macro))

(defun bedit-start-with-symbols-like-this ()
  (interactive)
  (if-let* ((symbol (thing-at-point 'symbol t)))
      (let ((search (format "\\_<%s\\_>" (regexp-quote symbol))))
        (bedit--create-recording-buffer)
        (setq bedit--editing-search
              (lambda (bound)
                (search-forward-regexp search bound t))
              bedit--editing-post-macro
              nil)
        (setq bedit--editing-search-offset
              (save-mark-and-excursion
                (let ((p (point)))
                  (forward-symbol -1)
                  (search-forward-regexp search)
                  (- p (point)))))
        (bedit--start-macro))
    (message "No symbol found at point")))

(defun bedit-start-with-lines (n)
  (interactive "P")
  (bedit--create-recording-buffer)
  (setq bedit--editing-search (lambda (bound) (search-forward-regexp "^" bound t n))
        bedit--editing-post-macro
        (lambda ()
          (goto-char (line-end-position))
          (= (point) (point-max)))
        bedit--editing-search-offset (- (point) (line-beginning-position)))
  (bedit--start-macro))

(defun bedit-start-with-search-like-this (beg end)
  (interactive "r")
  (let ((search (buffer-substring-no-properties beg end)))
    (bedit--create-recording-buffer)
    (setq bedit--editing-search (lambda (bound) (search-forward search bound t))
          bedit--editing-post-macro nil)
    (setq bedit--editing-search-offset (if (= (point) beg) (- beg end) 0))
    (bedit--start-macro)))

(defun bedit-start-with-chars-like-this ()
  (interactive)
  (let ((c (char-after)))
    (bedit--create-recording-buffer)
    (setq bedit--editing-search (lambda (bound) (search-forward (char-to-string c) bound t))
          bedit--editing-post-macro nil
          bedit--editing-search-offset -1)
    (bedit--start-macro)))

(defun bedit-quit ()
  (interactive)
  (delete-overlay mouse-secondary-overlay)
  (when defining-kbd-macro
    (keyboard-quit)))

(defun bedit--scope-start ()
  (or (overlay-start mouse-secondary-overlay)
      (point-min)))

(defun bedit--scope-end ()
  (or (overlay-end mouse-secondary-overlay)
      (point-max)))

(defun bedit--finish-recording ()
  "This is an internal command, do not bind to your keymap."
  (interactive)
  (end-kbd-macro)
  (kill-buffer (current-buffer))
  (switch-to-buffer bedit--editing-buffer)
  (let ((start (bedit--scope-start))
        (m (make-marker))
        (case-fold-search nil)
        break)
    (set-marker m (point))
    (goto-char start)
    (dolist (m bedit-inhibit-modes)
      (funcall m -1))
    (unwind-protect
        (bedit--wrap-collapse-undo
          (while (and (not break)
                      (funcall bedit--editing-search (bedit--scope-end)))
            (save-mark-and-excursion
              (forward-char bedit--editing-search-offset)
              (call-interactively #'kmacro-call-macro))
            (when bedit--editing-post-macro
              (when (funcall bedit--editing-post-macro)
                (setq break t)))))
      (progn
        (dolist (m bedit-inhibit-modes)
          (funcall m 1))
        (delete-overlay mouse-secondary-overlay)
        (goto-char (marker-position m))
        (set-marker m nil)))))

(defun bedit--cancel-recording ()
  "This is an internal command, do not bind to your keymap."
  (interactive)
  (message "Recording cancelled")
  (let ((buf (current-buffer)))
    (switch-to-buffer bedit--editing-buffer)
    (kill-buffer buf))
  (when defining-kbd-macro
    (keyboard-quit)))

(provide 'bedit)

;; -*- lexical-binding: t; -*-

(defvar bedit-overlays nil
  "A list of overlays that represent the places we gonna to edit.")

(defvar bedit--editing-search nil
  "The type of current editing thing.")

(defvar bedit--editing-search-offset nil
  "The offset between after-search position and the original position.")

(defvar bedit--editing-buffer nil
  "The buffer name we are editing.")

(defcustom bedit-inhibit-modes '(font-lock-mode)
  "Disable these modes during execution.")

(defface edit-curosr
  '((((class color) (background dark))
     (:foreground "grey40"))
    (((class color) (background light))
     (:foreground "grey60")))
  "Face for cursors in bedit-mode."
  :group 'bedit)

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
    ;; Kill the old existing buffer first
    (when-let* ((buf (get-buffer bufname)))
      (kill-buffer buf))
    (let ((buf (get-buffer-create bufname)))
      (switch-to-buffer buf)
      (insert-buffer orig-buf)
      (setq bedit--editing-buffer (buffer-name orig-buf))
      (setq-local major-mode mm)
      (goto-char p)
      buf)))

(defmacro bedit--require-secondary-selection (&rest body)
  `(if (not (overlayp mouse-secondary-overlay))
       (message "Secondary selection is required")
     ,@body))

(defun bedit--start-macro ()
  (local-set-key [remap kmacro-end-or-call-macro] 'bedit--finish-recording)
  (local-set-key [remap kmacro-end-macro] 'bedit--finish-recording)
  (local-set-key [remap kmacro-cancel-edit] 'bedit--cancel-recording)
  (call-interactively #'kmacro-start-macro))

(defun bedit-start-with-symbols-like-this ()
  (interactive)
  (bedit--require-secondary-selection
   (if-let* ((symbol (thing-at-point 'symbol t)))
       (let ((search (format "\\_<%s\\_>" (regexp-quote symbol))))
         (bedit--create-recording-buffer)
         (setq bedit--editing-search
               (lambda (bound)
                 (search-forward-regexp search bound t)))
         (setq bedit--editing-search-offset
               (save-mark-and-excursion
                 (let ((p (point)))
                   (forward-symbol -1)
                   (search-forward-regexp search)
                   (- p (point)))))
         (bedit--start-macro))
     (message "No symbol found at point"))))

(defun bedit-start-with-beginning-of-lines ()
  (interactive)
  (bedit--require-secondary-selection
   (bedit--create-recording-buffer)
   (setq bedit--editing-search
         (lambda (bound)
           (search-forward-regexp "^" bound t))
         bedit--editing-search-offset (- (point) (line-beginning-position)))
   (bedit--start-macro)))

(defun bedit-start-with-end-of-lines ()
  (interactive)
  (bedit--require-secondary-selection
   (bedit--create-recording-buffer)
   (setq bedit--editing-search
         (lambda (bound)
           (goto-char (1+ (point)))
           (search-forward-regexp "$" bound t))
         bedit--editing-search-offset (- (point) (line-end-position)))
   (bedit--start-macro)))

(defun bedit-start-with-search-like-this (beg end)
  (interactive "r")
  (message "beg: %s, end: %s" beg end)
  (bedit--require-secondary-selection
   (let ((search (buffer-substring-no-properties beg end)))
     (bedit--create-recording-buffer)
     (setq bedit--editing-search
           (lambda (bound)
             (search-forward search bound t)))
     (setq bedit--editing-search-offset
           (if (= (point) beg)
               (- beg end)
             0))
     (bedit--start-macro))))

(defun bedit-start-with-chars-like-this ()
  (interactive)
  (bedit--require-secondary-selection
   (let ((c (char-after)))
     (bedit--create-recording-buffer)
     (setq bedit--editing-search
           (lambda (bound)
             (search-forward (char-to-string c) bound t))
           bedit--editing-search-offset -1)
     (bedit--start-macro))))

(defun bedit-quit ()
  (interactive)
  (delete-overlay mouse-secondary-overlay)
  (when defining-kbd-macro
    (keyboard-quit)))

(defun bedit--finish-recording ()
  "This is an internal command, do not bind to your keymap."
  (interactive)
  (end-kbd-macro)
  (kill-buffer (current-buffer))
  (switch-to-buffer bedit--editing-buffer)
  (let ((start (overlay-start mouse-secondary-overlay))
        (m (make-marker)))
    (set-marker m (point))
    (goto-char start)
    (dolist (m bedit-inhibit-modes)
      (funcall m -1))
    (bedit--wrap-collapse-undo
      (while (and (< (point) (overlay-end mouse-secondary-overlay))
                  (funcall bedit--editing-search (overlay-end mouse-secondary-overlay)))
        (let ((m1 (make-marker)))
          (set-marker m1 (point))
          (forward-char bedit--editing-search-offset)
          (call-interactively #'kmacro-call-macro)
          (goto-char (marker-position m1))
          (set-marker m1 nil))))
    (dolist (m bedit-inhibit-modes)
      (funcall m 1))
    (delete-overlay mouse-secondary-overlay)
    (goto-char (marker-position m))
    (set-marker m nil)))

(defun bedit--cancel-recording ()
  "This is an internal command, do not bind to your keymap."
  (interactive)
  (when defining-kbd-macro
    (keyboard-quit))
  (let ((buf (current-buffer)))
    (switch-to-buffer bedit--editing-buffer)
    (kill-buffer buf)))

(define-prefix-command 'bedit-mode-map)
(define-key bedit-mode-map (kbd "SPC") 'bedit-region-to-secondary-selection)
(define-key bedit-mode-map (kbd "d") 'bedit-defun-to-secondary-selection)
(define-key bedit-mode-map (kbd "p") 'bedit-paragraph-to-secondary-selection)
(define-key bedit-mode-map (kbd "l") 'bedit-line-to-secondary-selection)
(define-key bedit-mode-map (kbd "q") 'bedit-quit)
(define-key bedit-mode-map (kbd ".") 'bedit-start-with-symbols-like-this)
(define-key bedit-mode-map (kbd "s") 'bedit-start-with-search-like-this)
(define-key bedit-mode-map (kbd "a") 'bedit-start-with-beginning-of-lines)
(define-key bedit-mode-map (kbd "e") 'bedit-start-with-end-of-lines)
(define-key bedit-mode-map (kbd "c") 'bedit-start-with-chars-like-this)

(define-key global-map (kbd "C-M-g") bedit-mode-map)

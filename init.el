;;; -*- lexical-binding: t -*-

;; Early load org-mode
(straight-use-package 'org)
(straight-use-package 'diminish)
(require 'org)
(require 'diminish)

;; Define helper command for reloading configuration
(defun meomacs-refresh ()
  "Refresh and tangle configuration."
  (interactive)
  (meomacs-load-config "private" t)
  (meomacs-load-config "laf" t)
  (meomacs-load-config "editor" t)
  (meomacs-load-config "writing" t)
  (meomacs-load-config "programming" t)
  (meomacs-load-config "addons" t)
  (meomacs-load-config "llm" t))

;; Define helper command for open configuration file.
(defun meomacs-open-configuration ()
  "Open meomacs.org under `user-emacs-directory'."
  (interactive)
  (let ((config (completing-read "Open configuration: "
				 '("private"
				   "laf"
				   "editor"
				   "writing"
				   "programming"
				   "addons"
                                   "llm")
				 nil
				 t)))
    (find-file (expand-file-name (format "%s.org" config) user-emacs-directory))))

;; Define helper macro to parse key binding tables
(defmacro meomacs-keymap-table (keymap table)
  `(progn
     (unless (boundp (quote ,keymap))
       (defvar ,keymap (make-keymap)))
     (let ((parse-and-def (lambda (x)
                            (keymap-set ,keymap (car x) (intern (cadr x))))))
       (mapcar parse-and-def ,table))
     (defalias (quote ,keymap) ,keymap)))

(global-set-key (kbd "<f9>") 'meomacs-open-configuration)
(global-set-key (kbd "<f12>") 'meomacs-refresh)

;; Load main configuration
(meomacs-load-config "editor")
(meomacs-load-config "writing")
(meomacs-load-config "programming")
(meomacs-load-config "addons" t)
(meomacs-load-config "llm" t)

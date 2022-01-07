;;; -*- lexical-binding: t -*-

;; Define helper command for reloading configuration
(defun meomacs-refresh ()
  "Refresh and tangle configuration."
  (interactive)
  (meomacs-load-config "laf" t)
  (meomacs-load-config "meomacs" t)
  (meomacs-load-config "programming" t))

;; Define helper command for open configuration file.
(defun meomacs-open-configuration ()
  "Open meomacs.org under `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "meomacs.org" user-emacs-directory)))

(global-set-key (kbd "<f9>") 'meomacs-open-configuration)
(global-set-key (kbd "<f12>") 'meomacs-refresh)

;; Load main configuration
(meomacs-load-config "meomacs")
(meomacs-load-config "programming")

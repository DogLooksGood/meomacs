;;; -*- lexical-binding: t -*-

;; Define helper command for reloading configuration
(defun meomacs-refresh ()
  "Refresh and tangle configuration."
  (interactive)
  (meomacs-load-config "laf" t)
  (meomacs-load-config "private" t)
  (meomacs-load-config "writing" t)
  (meomacs-load-config "programming" t)
  (meomacs-load-config "addons" t))

;; Define helper command for open configuration file.
(defun meomacs-open-configuration ()
  "Open meomacs.org under `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "meomacs.org" user-emacs-directory)))

(global-set-key (kbd "<f9>") 'meomacs-open-configuration)
(global-set-key (kbd "<f12>") 'meomacs-refresh)

;; Load main configuration
(meomacs-load-config "writing")
(meomacs-load-config "programming")
(meomacs-load-config "addons" t)

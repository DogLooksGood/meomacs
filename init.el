;;; -*- lexical-binding: t -*-

;; disabling GC during initialization
(setq gc-cons-threshold most-positive-fixnum)

;; Ensure we have correct user-emacs-directory
;; The folder of meomacs can be placed anywhere, and started with
;;   emacs -q -l /path/to/meomacs/init.el
(setq user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))

;; Define helper command for reloading configuration
(defun meomacs-refresh ()
  "Refresh and tangle configuration."
  (interactive)
  (let ((source (expand-file-name "meomacs.org" user-emacs-directory))
        (target (expand-file-name "tangle.el" user-emacs-directory)))
    (require 'org)
    (require 'ob)
    (org-babel-tangle-file source target)
    (load-file target)))

;; Define helper command for open configuration file.
(defun meomacs-open-configuration ()
  "Open meomacs.org under `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "meomacs.org" user-emacs-directory)))

(global-set-key (kbd "<f9>") 'meomacs-open-configuration)
(global-set-key (kbd "<f12>") 'meomacs-refresh)

;; Detecting tangle output
;; Load existing tangle file if it exists.
(let ((inhibit-message t)
      (target (expand-file-name "tangle.el" user-emacs-directory)))
  (if (file-exists-p target)
      (load-file target)
    (meomacs-refresh)))

;; enable GC
(setq gc-cons-threshold #x8000000)

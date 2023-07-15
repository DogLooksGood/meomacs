;; Disable GC during initialization(for the case, early-init.el is not used)
(setq gc-cons-threshold most-positive-fixnum)

;; Ensure we have correct user-emacs-directory
;; The folder of meomacs can be placed anywhere, and started with
;;   emacs -q -l /path/to/meomacs/init.el
(setq user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))

;; Define configuration loader helper
(defun meomacs-load-config (config-name &optional force-tangle)
  "Load configuration by CONFIG-NAME.
Firstly try load the CONFIG-NAME.el file.
If the elisp file was not found, tangle the CONFIG-NAME.org to generate one.

If FORCE-TANGLE is non-nil, always tangle before load."
  (let* ((source (expand-file-name (format "%s.org" config-name) user-emacs-directory))
	 (tangle-dir (expand-file-name "tangle" user-emacs-directory))
	 (target (expand-file-name (format "%s.el" config-name) tangle-dir)))
    (when (file-exists-p source)
      (make-directory tangle-dir t)
      (when (or force-tangle (not (file-exists-p target)))
	(require 'org)
	(require 'ob)
	(org-babel-tangle-file source target))
      (load-file target))))

;; Prepare private.org when not exist.

(unless (file-exists-p (expand-file-name "private.org" user-emacs-directory))
  (copy-file
   (expand-file-name "private_template.org" user-emacs-directory)
   (expand-file-name "private.org" user-emacs-directory)))

;; Setup straight as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Straight configs
(setq straight-vc-git-default-clone-depth 1)

;; Use GCMH as GC config.
(straight-use-package 'gcmh)
(require 'gcmh)
(gcmh-mode 1)

;; Dump flag
(defvar meomacs-dump nil)

;; Load configurations
(meomacs-load-config "private")
(meomacs-load-config "laf")

;; Enable the first theme in `meomacs-themes'
(unless custom-enabled-themes
  (meomacs-load-theme))

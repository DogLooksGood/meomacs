;; Disable GC during initialization(for the case, early-init.el is not used)
(setq gc-cons-threshold most-positive-fixnum)

;; Ensure we have correct user-emacs-directory
;; The folder of meomacs can be placed anywhere, and started with
;;   emacs -q -l /path/to/meomacs/init.el
(setq user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))

;; Enable GC after initialization
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold #x80000000)))

;; Define configuration loader helper
(defun meomacs-load-config (config-name &optional force-tangle)
  "Load configuration by CONFIG-NAME.
Firstly try load the CONFIG-NAME.el file.
If the elisp file was not found, tangle the CONFIG-NAME.org to generate one.

If FORCE-TANGLE is non-nil, always tangle before load."
  (let* ((source (expand-file-name (format "%s.org" config-name) user-emacs-directory))
	 (tangle-dir (expand-file-name "tangle" user-emacs-directory))
	 (target (expand-file-name (format "%s.el" config-name) tangle-dir)))
    (make-directory tangle-dir t)
    (when (or force-tangle (not (file-exists-p target)))
      (require 'org)
      (require 'ob)
      (org-babel-tangle-file source target))
    (load-file target)))

;; Detecting look & feel configuration
(meomacs-load-config "laf")

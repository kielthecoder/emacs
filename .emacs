;; Emacs package management
(require 'package)

;; Add repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Activate package autoloads
(package-initialize)

;; List the packages I use
(defvar my-packages '(slime))

;; Fetch the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; How do we handle backups?
(make-directory "~/.emacs.d/backups" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)

;; Hide the splash screen
(setq inhibit-splash-screen t)

;; Disable toolbar but keep the menu bar
(tool-bar-mode -1)
(menu-bar-mode 1)

;; Configure SLiME
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

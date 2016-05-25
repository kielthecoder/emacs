;; Emacs package management
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
	       '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(defvar my-packages '(better-defaults
                      slime))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; I already know I'm using Emacs, hide the splash screen
(setq inhibit-splash-screen t)

;; Use custom Solarized theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized t)

;; Setup SLIME (still need to start with M-x slime)
(setq inferior-lisp-program "C:/SBCL/sbcl.exe")
(setq slime-contribs '(slime-fancy))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Now we can enable the theme in dark mode
(enable-theme 'solarized)

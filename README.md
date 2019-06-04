
# Table of Contents

1.  [Start Up](#org33fc0dc)
2.  [Personal Information](#orgb518d3e)
3.  [Backups](#orgd4fd3e9)
4.  [Windows](#org547c546)
5.  [Org-mode](#org5936de2)
    1.  [Exports](#org40e648f)
6.  [SLiME](#org991f434)
7.  [Theme](#org293ec92)

This is my Emacs configuration.  I'm using [Sacha Chua's Emacs configuration](https://pages.sachachua.com/.emacs.d/Sacha.html) as a reference since it seems to be the best!


<a id="org33fc0dc"></a>

# Start Up

First, we require package management.

    (require 'package)

List the repositories we use.

    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (when (< emacs-major-version 24)
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
    (package-initialize)

List the packages I'm currently using.

    (defvar my-packages '(slime))

Fetch the list of available packages.

    (unless package-archive-contents
      (package-refresh-contents))

Install missing packages automatically.

    (dolist (pkg my-packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))


<a id="orgb518d3e"></a>

# Personal Information

    (setq user-full-name "Kiel Lofstrand")
    (setq user-mail-address "kiel.lofstrand@gmail.com")


<a id="orgd4fd3e9"></a>

# Backups

Move backup files to a dedicated directory where they're easier to find:

    (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
    (setq backup-by-copying t)
    (setq version-control t)
    (setq delete-old-versions t)


<a id="org547c546"></a>

# Windows

Get rid of the toolbar, it takes up too much space.

    (tool-bar-mode -1)

Keep the menu bar, it's handy sometimes.

    (menu-bar-mode 1)


<a id="org5936de2"></a>

# Org-mode

    (require 'org)


<a id="org40e648f"></a>

## Exports

Setup other export backends not loaded by default.

    (require 'ox-md)


<a id="org991f434"></a>

# SLiME

Configure our lisp interpretter.

    (setq inferior-lisp-program "/usr/bin/sbcl")

Which SLiME features are we using?

    (setq slime-contribs '(slime-fancy))


<a id="org293ec92"></a>

# Theme

    (custom-set-variables
     '(custom-enabled-themes (quote (tango-dark))))

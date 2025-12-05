;;; init.el --- Cleaned Emacs Config -*- lexical-binding: t -*-

;;; 1. PACKAGE MANAGER SETUP
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))

;; Guix integration
(add-to-list 'load-path "/home/sibl/.guix-profile/share/emacs/site-lisp")
(when (fboundp 'guix-emacs-autoload-packages)
  (guix-emacs-autoload-packages))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; 2. CORE EMACS SETTINGS
(use-package emacs
  :init
  ;; UI cleanup
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (setq inhibit-startup-screen t
        use-dialog-box nil
        ring-bell-function 'ignore)
  
  ;; Font (Fallback if daemon/client setup differs)
  (add-to-list 'default-frame-alist '(font . "Terminess Nerd Font 14"))
  (add-to-list 'default-frame-alist '(undecorated . t))
  (add-to-list 'default-frame-alist '(alpha-background . 90))

  ;; Scrolling
  (setq scroll-conservatively 101
        scroll-margin 0)

  ;; File handling & Backups (Keep main dir clean)
  (setq make-backup-files t
        backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
        auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t))
        create-lockfiles nil)
  (save-place-mode t)
  (savehist-mode t)
  (recentf-mode t)
  (global-auto-revert-mode t)

  ;; Auth sources
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")
        epg-pinentry-mode 'loopback)

  ;; Interaction
  (setq confirm-kill-emacs #'yes-or-no-p)
  (defalias 'yes-or-no-p 'y-or-n-p) ; Use y/n instead of yes/no

  ;; Custom file (Don't clutter init.el with auto-generated vars)
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

;;; 3. UI & THEMES
(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package nerd-icons)
(use-package olivetti)

;; Window navigation bindings
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;;; 4. COMPLETION SYSTEM
(use-package vertico
  :init (vertico-mode t)
  :custom
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t))

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match 'separator))

(use-package easysession)

;;; 5. DEVELOPMENT & GIT
(use-package magit
  :bind ("C-c g" . magit-status)
  :hook (after-save . magit-after-save-refresh-status))

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package eglot
  :hook ((zig-mode ruby-mode cperl-mode c-mode-common) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs '(cperl-mode . ("pls")))
  (defalias 'start-lsp-server #'eglot))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . electric-pair-mode)))

(use-package dwim-shell-command)
(setq comint-process-echoes t) ;; fix print command in *shell*

;; Language specific modes
(use-package go-mode)
(use-package json-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package zig-mode)
(use-package guix)

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           (setq c-default-style "linux"
                                 c-basic-offset 8
                                 tab-width 8
                                 indent-tabs-mode t)
                           (electric-indent-local-mode -1))))

(use-package cperl-mode
  :custom
  (cperl-close-paren-offset (- cperl-indent-level))
  (cperl-indent-parens-as-block t)
  :config
  (custom-set-faces
   '(cperl-array-face ((t (:weight normal))))
   '(cperl-hash-face ((t (:weight normal))))))

(use-package perl-doc)

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter)) 

;; TRAMP Configuration
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (mapc (lambda (path) (add-to-list 'tramp-remote-path path))
        '("/bin" "/usr/bin" "/usr/local/bin" "/opt/bin" "/opt/homebrew/bin")))

;;; 6. ORG MODE
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . org-superstar-mode))
  :bind
  (:map org-mode-map
        ("M-S-<left>"  . org-metaleft)
        ("M-S-<right>" . org-metaright)
        ("M-S-<up>"    . org-metaup)
        ("M-S-<down>"  . org-metadown))
  :custom
  (org-latex-compiler "pdflatex")
  (org-html-validation-link nil)
  (org-babel-python-command "python3")
  :config
  ;; Unbind Meta-arrow keys in Org to allow windmove to work
  (define-key org-mode-map (kbd "<M-left>") nil)
  (define-key org-mode-map (kbd "<M-right>") nil)
  (define-key org-mode-map (kbd "<M-up>") nil)
  (define-key org-mode-map (kbd "<M-down>") nil)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t) (perl . t) (sql . t) (C . t) (lua . t)
     (shell . t) (python . t) (scheme . t) (sqlite . t) (emacs-lisp . t))))

(use-package org-superstar)
(use-package htmlize)

;;; 7. APPLICATIONS & MEDIA
(use-package dired
  :ensure nil
  :custom (dired-kill-when-opening-new-dired-buffer t))

(use-package emms
  :config
  (emms-all)
  (emms-default-players)
  (setq emms-player-list '(emms-player-vlc)))

(use-package eat
  :load-path "/home/sibl/Documents/git/emacs-eat"
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-mouse t))

(use-package erc
  :custom
  (erc-server "irc.libera.chat")
  (erc-port 6697)
  (erc-nick "azval")
  (erc-user-full-name "azval")
  (erc-use-auth-source-for-nickserv-password t)
  (erc-prompt-for-nickserv-password nil)
  (erc-track-shorten-start 8)
  (erc-hide-list '("JOIN" "PART" "NICK" "QUIT"))
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  :config
  (erc-services-mode 1))

;;; EMAIL (Gnus)
;;(use-package gnus
;;  :config
;;  (setq user-full-name "Simon Blacks"
;;        user-mail-address "you@example.com")
;;  (setq gnus-select-method '(nnnil nil))
;;  (setq gnus-secondary-select-methods
;;	'((nnimap "MyMail"
;;                 (nnimap-address "smtp.freesmtpservers.com")
;;                 (nnimap-server-port 25)
;;                 (nnimap-stream network))))
;;  (setq send-mail-function 'smtpmail-send-it
;;        message-send-mail-function 'smtpmail-send-it
;;        smtpmail-smtp-server "smtp.freesmtpservers.com"
;;        smtpmail-smtp-service 25
;;        smtpmail-stream-type 'network)
;;  (setq gnus-inhibit-images nil)
;;  (setq mm-text-html-renderer 'shr)
;;  ;; Clean up the summary view (Date | Sender | Subject)
;;  (setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n")
;;  (setq gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
;;  ;; Don't get the first article automatically:
;;  (setq gnus-auto-select-first nil)
;;  (setq smiley-style 'medium)
;;  ;; Show more MIME-stuff:
;;  (setq gnus-mime-display-multipart-related-as-mixed t)
;;  (setq gnus-asynchronous t
;;      gnus-use-cache t
;;      gnus-use-header-prefetch t))

;;; 8. CUSTOM FUNCTIONS
(defun mosh-connect (host)
  "Connect to HOST via mosh using TRAMP configuration."
  (interactive
   (list
    (completing-read
     "mosh host: "
     (flatten-tree
      (remove '(nil nil) (remove nil (tramp-parse-sconfig "~/.ssh/config")))))))
  (message "host is %s" host)
  (defvar-local buffer-name (format "*%s*" host))
  (switch-to-buffer (eat (format "mosh %s" host)))
  (rename-buffer buffer-name))

(defun term-transparent-frame ()
  "Remove background color for transparent terminal."
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified-bg")
    (set-face-background 'line-number "unspecified-bg")))
(add-hook 'tty-setup-hook 'term-transparent-frame)

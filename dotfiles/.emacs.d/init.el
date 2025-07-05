;; package setup
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; theme
(use-package catppuccin-theme)
(load-theme 'catppuccin :no-confirm)
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package olivetti)

(use-package nerd-icons)

;; session handling
(use-package easysession)

;; command completion
(use-package vertico
  :config
  ;; Enable completion by narrowing
  (vertico-mode t)
  (setq read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t
	completion-ignore-case t))

;; lsp support
(use-package eglot
  :config
  ;; Enable line numbering in `prog-mode'
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  ;; Automatically pair parentheses
  (electric-pair-mode t)
  ;; Enable LSP support by default in programming buffers
  (add-hook 'prog-mode-hook #'eglot-ensure)
  (add-hook 'zig-mode-hook #'eglot-ensure)
  (add-hook 'ruby-mode-hook #'eglot-ensure)
  (add-hook 'cperl-mode-hook #'eglot-ensure)
  ;; Create a memorable alias for `eglot-ensure'.
  (defalias 'start-lsp-server #'eglot))

;; Pop-up completion
(use-package corfu
  :config
  ;; Enable auto completion and configure quitting
  (setq corfu-auto t
	corfu-auto-prefix 1 ;; propose after typing 1 char
	corfu-quit-no-match 'separator) ;; or t
  ;; Enable autocompletion by default in programming buffers
  (add-hook 'prog-mode-hook #'corfu-mode)
  (add-hook 'org-mode-hook #'corfu-mode))

;; Git client
(use-package magit
  :config
  ;; Bind the `magit-status' command to a convenient key.
  (global-set-key (kbd "C-c g") #'magit-status))
;; auto refresh magit on buffer save
(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
;; git mark on buffer (next to line number)
(use-package diff-hl)
(global-diff-hl-mode)

(use-package go-mode)
(use-package json-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package zig-mode)
(use-package cperl-mode
  :config
  (defalias 'perl-mode 'cperl-mode)
  (setq cperl-close-paren-offset (- cperl-indent-level))
  (setq cperl-indent-parens-as-block t)
  (custom-set-faces
   '(cperl-array-face ((t (:weight normal))))
   '(cperl-hash-face ((t (:weight normal))))))
(use-package perl-doc)
;; better for perl lsp
(add-to-list 'eglot-server-programs '(perl-mode . ("pls")))
;;(add-to-list 'eglot-server-programs `(perl-mode . ("/usr/local/bin/perlnavigator", "--stdio")))
(use-package org
  :config
  (setq-default org-latex-compiler "pdflatex")
  (setq org-html-validation-link nil)
  ; avoid indent in src block
  ; (setq-default org-edit-src-content-indentation 0)
  (use-package org-superstar)
  (add-hook 'org-mode-hook #'olivetti-mode)
  (add-hook 'org-mode-hook #'org-superstar-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)
  ;; export org to html
  (use-package htmlize)
  
  ;; Org babel setting (src/code block)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (perl . t)
     (sql . t)
     (C . t)
     (lua . t)
     (shell . t)
     (python . t)
     (emacs-lisp t)))
  ;; otherwise fail using "python"
  (setq org-babel-python-command "python3"))

(use-package eat
  :config
  ;; Close the terminal buffer when the shell terminates.
  (setq eat-kill-buffer-on-exit t)
  ;; Enable mouse-support.
  (setq eat-enable-mouse t))

;; music player
(use-package emms)
(emms-all)
(emms-default-players)
(setq emms-player-list '(emms-player-vlc))
;;      emms-info-functions '(emms-info-native))

;;;;;;;;; other setup ;;;;;;;;;;;;;;

;; homepage
;; (add-hook 'after-init-hook 'recentf-open-files)
;; (inhibit-startup-screen t)

;; font setup
;;(set-frame-font "FiraCode Nerd Font 13" nil t)
(set-frame-font "Terminess Nerd Font 14" nil t)

;; hide toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq-default cursor-in-non-selected-windows nil)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

;; ERC/IRC
(use-package erc
  :config
  (setq-default erc-server "irc.libera.chat")
  (setq-default erc-nick "lexiz")
  (setq-default erc-user-full-name "lexiz")
  (setq-default erc-track-shorten-start 8)
  (setq erc-hide-list '("JOIN" "PART" "NICK" "QUIT"))
  (setq-default erc-autojoin-channels-alist '(("irc.libera.chat" "#dimsumlabs" "#emacs" "#guix" "#perl")))
  (setq-default erc-server-reconnect-attempts 5)
  (setq-default erc-server-reconnect-timeout 3)
  (setq-default erc-kill-buffer-on-part t)
  (setq-default erc-auto-query 'bury)
  (setq-default erc-fill-static-center 18))

;; GPG settings
(setq epg-pinentry-mode 'loopback)

;; smooth scrolling
(setq scroll-conservatively 101)

;; remove window decoration and add transparency
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; update buffer on file change
(global-auto-revert-mode)

;; bindings
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;; custom functions
(defun mosh-connect (host)
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

;; auth TRAMP setup
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
(setq tramp-remote-path (list "/bin" "/usr/bin" "/sbin" "/usr/sbin"
			      "/usr/local/bin" "/usr/local/sbin"
			      "/local/bin" "/local/freeware/bin"
			      "/local/gnu/bin" "/usr/freeware/bin"
			      "/usr/pkg/bin" "/usr/contrib/bin"
			      "/opt/bin" "/opt/sbin" "/opt/local/bin"
			      "/opt/homebrew/bin" "/opt/homebrew/sbin"
			      'tramp-own-remote-path))
;; Configure Emacs to store autosave files in a specific local directory
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save/" t)))
;; Optionally, also configure backup files
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/backups/")))
;; Create the directory if it doesn't exist (optional, Emacs usually creates it)
(unless (file-exists-p "~/.emacs.d/auto-save/")
  (make-directory "~/.emacs.d/auto-save/" t))
;; For backup files too if you enabled them above
(unless (file-exists-p "~/.emacs.d/backups/")
  (make-directory "~/.emacs.d/backups/" t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(auth-source-save-behavior 'ask)
 '(erc-prompt-for-password nil)
 '(org-edit-src-content-indentation 0)
 '(package-selected-packages
   '(auctex catppuccin-theme corfu diff-hl doom-modeline easysession eat
	    emms exec-path-from-shell go-mode htmlize json-mode
	    lua-mode magit markdown-mode mood-line nano-modeline
	    nano-theme nim-mode olivetti org-modern org-superstar
	    pyenv-mode rust-mode vertico zig-mode))
 '(tab-bar-mode 1)
 '(tab-bar-show 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:weight normal))))
 '(cperl-hash-face ((t (:weight normal)))))

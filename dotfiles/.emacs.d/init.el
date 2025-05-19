;; package setup
(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("GNU" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; homepage
;(add-hook 'after-init-hook 'recentf-open-files)
;(inhibit-startup-screen t)

;; font setpu
(set-frame-font "FiraCode Nerd Font 12" nil t)

;; hide toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; catppuccin theme
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))
(load-theme 'catppuccin :no-confirm)

(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))

;; Use whatever the default monospace font is
(setq font-use-system-font t)

;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

;; Enable completion by narrowing
(vertico-mode t)
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)
(add-hook 'zig-mode-hook 'eglot-ensure)

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))
;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-auto-prefix 1 ;; propose after typing 1 char
      corfu-quit-no-match 'separator) ;; or t

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;;; Go Support
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

;;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;;; Lua Support
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;; Zig support
(unless (package-installed-p 'zig-mode)
  (package-install 'zig-mode))

;;; Perl support
(unless (package-installed-p 'cperl-mode)
  (package-install 'cperl-mode))
(unless (package-installed-p 'perl-doc)
  (package-install 'perl-doc))
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-parens-as-block t)
;(setq cperl-close-paren-offset (- cperl-indent-level))

;;; Org mode
(setq-default org-latex-compiler "pdflatex")
(setq org-html-validation-link nil)

;;; Org babel setting
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
(setq org-babel-python-command "python3") ;; otherwise fail using "python"
(add-hook 'org-mode-hook 'olivetti-mod)

;;; In-Emacs Terminal Emulation
(unless (package-installed-p 'eat)
  (package-install 'eat))

;; Close the terminal buffer when the shell terminates.
(setq eat-kill-buffer-on-exit t)

;; Enable mouse-support.
(setq eat-enable-mouse t)

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
(setq erc-server "irc.libera.chat"
      erc-nick "azz"
      erc-user-full-name "azz"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#dimsumlabs" "#emacs" "#guix" "#perl" "#zig"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)

;; smooth scrolling
(setq scroll-conservatively 101)

;; start window big size
; (add-hook 'window-setup-hook 'toggle-frame-maximized t) doesnt seem to work
;; modeline bar
(doom-modeline-mode 1)

;; bindings
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(package-selected-packages
   '(catppuccin-theme corfu doom-modeline easysession eat go-mode htmlize
		      json-mode lua-mode magit markdown-mode mood-line
		      nano-modeline nano-theme org-mode org-modern
		      org-superstar perl-doc pyenv-mode vertico
		      zig-mode))
 '(tab-bar-mode nil)
 '(tab-bar-show 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

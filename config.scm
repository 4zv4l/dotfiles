(define-module (guix-home-config)
  #:use-module (ice-9 string-fun)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system shadow))

(define %PATH
  (string-replace-substring
    (string-join
      (list "$HOME/.local/bin"
            "$HOME/perl5/bin"
            "$PATH") ":")
    "$HOME" (getenv "HOME")))

(define %my-shepherd-services
  (list (shepherd-service
          (documentation "Run Syncthing.")
          (provision '(syncthing))
          (start #~(make-forkexec-constructor
                     '("syncthing" "-no-browser")
                     #:log-file (string-append (getenv "HOME")
                                               "/log/syncthing.log")))
          (stop #~(make-kill-destructor)))))

(home-environment
  (packages
    (specifications->packages
      (list "tmux"
            "zoxide"
            ;;"neovim" version from guix is not yet 10
            "bat"
            "lsd"
            "senpai"
            "perl"
            "guile"
            "guile-lsp-server"
            "zig"
            "zig-zls"
            "nushell"
            "zip"
            "unzip"
            "mtr"
            "ltrace"
            "strace"
            "nmap")))

  (services
    (cons*
      ;; basic bash setup
      (service home-bash-service-type
        (home-bash-configuration
          (environment-variables
            `(("PATH"   . ,%PATH)
              ("PS1"    . "$(history -a;history -n)$PS1")
              ("EDITOR" . "nvim")))
          (aliases
            '(("cat" . "bat")
              ("ls"  . "lsd")
              ("v"   . "nvim")))))
      ;; copy dotfiles as symlinks
	  (service home-dotfiles-service-type
	    (home-dotfiles-configuration
	    (directories (list "./dotfiles"))))
      ;; shepherd services
      (service home-shepherd-service-type
        (home-shepherd-configuration
          (services %my-shepherd-services)))
      %base-home-services)))

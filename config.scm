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
   (list "fortunes-jkirchartz"
         "cowsay"
         "lolcat"
         "tmux"
         "zoxide"
         "fzf"
         ;;"neovim" version from guix is not yet 10
         "emacs"
         "bat"
         "lsd"
         "senpai"
         "perl"
         "guile"
         "guile-lsp-server"
         "zig"
         "zig-zls"
         "gcc-toolchain"
         "pkg-config"
         "nushell"
         "zstd"
         "pigz"
         "zip"
         "unzip"
         "mtr"
         "ltrace"
         "strace"
         "nmap")))

 (services
  (cons*
   (service home-fish-service-type
            (home-fish-configuration
             (environment-variables
              `(("PATH"           . ,%PATH)             
                ("EDITOR"         . "nvim")))
             (aliases
              '(("cat" . "bat")
                ("ls"  . "lsd")
                ("v"   . "nvim")))
             (config
              (list (plain-file "rc"
                                (string-join
                                 (list "eval (perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"
                                       "zoxide init fish | source"
                                       "function fish_greeting; echo 'Hello sibl !';end")
				       ;; "function fish_greeting; fortune | cowsay | lolcat;end")
                                "\n"))))))
   (service home-bash-service-type
	    (home-bash-configuration
	     (environment-variables
	      `(("PATH"           . ,%PATH)
		("PS1"            . "$(history -a;history -n)$PS1")
		("HISTSIZE"       . "-1")
		("HISTFILESIZE"   . "-1")
		("EDITOR"         . "nvim")))
	     (aliases
	      '(("cat" . "bat")
		("ls"  . "lsd")
		("v"   . "nvim")))
	     (bashrc
	      (list (plain-file "rc"
				(string-join
				 (list "eval \"$(zoxide init bash)\""
				       "eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)")
				 "\n"))))))
   ;; copy dotfiles as symlinks
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories (list "dotfiles"))))
   ;; shepherd services
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (services %my-shepherd-services)))
   %base-home-services)))

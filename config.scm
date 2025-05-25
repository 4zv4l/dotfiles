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
   (list "cowsay"
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
              `(("NFORTUNE_DATABASE" . "$HOME/.local/share/fortunes/")
                ("PERL_MB_OPT" . "--install_base \"$HOME/perl5\"")
                ("PERL_MM_OPT" . "INSTALL_BASE=$HOME/perl5")
                ("LANG" . "C.UTF-8")
                ("LANGUAGE" . "C.UTF-8")
                ("LC_ALL" . "C.UTF-8")
                ("EDITOR"         . "nvim")))
             (aliases
              '(("cat" . "bat")
                ("ls"  . "lsd")
                ("v"   . "nvim")))
             (config
              (list (plain-file "rc"
                                (string-join
                                 (list "zoxide init fish | source"
                                       "eval \"$(guix package --search-paths -p ~/.config/guix/current -p ~/.guix-profile -p ~/.guix-home/profile -p /run/current-system/profile)\""
                                       "fish_add_path -P -a /bin"
                                       "fish_add_path -P -a /sbin"
                                       "fish_add_path -P -a /usr/bin"
                                       "fish_add_path -P -a /usr/sbin"
                                       "fish_add_path -P -a /usr/local/bin"
                                       "fish_add_path -P -a /usr/local/sbin"
                                       "fish_add_path -P -a ~/perl5/bin"
                                       "fish_add_path -P -a ~/.local/bin"
                                       "function fish_greeting; nfortune | cowsay | lolcat;end")
                                 "\n"))))))
   (service home-bash-service-type
            (home-bash-configuration
             (environment-variables
              `(("PATH"           . ,%PATH)
                ("PS1"            . "$(history -a;history -n)$PS1")
                ("HISTSIZE"       . "-1")
                ("HISTFILESIZE"   . "-1")
                ("NFORTUNE_DATABASE" . "$HOME/.local/share/fortunes/")
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

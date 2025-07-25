(define-module (guix-home-config)
  #:use-module (ice-9 string-fun)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services ssh)
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
         (stop #~(make-kill-destructor)))

        (shepherd-service
         (documentation "Run the Emacs daemon")
         (provision '(emacs-daemon))
         (start #~(make-forkexec-constructor
                   '("emacs" "--fg-daemon")
                   #:log-file (string-append (getenv "HOME")
                                             "/log/emacs.log")))
        (stop #~(make-kill-destructor)))))

(home-environment
(packages
 (specifications->packages
  (list "cowsay"
        "lolcat"
        "tmux"
        "zoxide"
        "fzf"
        "neovim" ; version from guix is not yet 10
        "emacs"
        "bat"
        "lsd"
        "senpai"
        "perl"
        "guile"
        "guile-lsp-server"
        "zig"
        "zig-zls"
	"fish"
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
        "make"
        "nmap")))

(services
 (cons*
  (service home-fish-service-type
           (home-fish-configuration
            (environment-variables
             `(("NFORTUNE_DATABASE" . "$HOME/.local/share/fortunes/")
               ("PERL_MB_OPT" . "--install_base \"$HOME/perl5\"")
               ("PERL_MM_OPT" . "INSTALL_BASE=$HOME/perl5")
               ("LANG" . "en_US.UTF-8")
               ("LANGUAGE" . "en_US.UTF-8")
               ("LC_ALL" . "en_US.UTF-8")
               ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:/home/sibl/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share")
               ("EDITOR"         . "nvim")))
            (aliases
             '(("cat" . "bat")
               ("ls"  . "lsd")
               ("emacst" . "emacsclient -t")
               ("em" . "emacsclient -t")
               ("v"   . "nvim")))
            (config
             (list (plain-file "rc"
                               (string-join
                                (list "eval \"$(guix package --search-paths -p ~/.config/guix/current -p ~/.guix-profile -p ~/.guix-home/profile -p /run/current-system/profile)\""
                                      "fish_add_path -P -a /bin"
                                      "fish_add_path -P -a /sbin"
                                      "fish_add_path -P -a /usr/bin"
                                      "fish_add_path -P -a /usr/sbin"
                                      "fish_add_path -P -a /usr/local/bin"
                                      "fish_add_path -P -a /usr/local/sbin"
                                      "fish_add_path -P -a ~/perl5/bin"
                                      "fish_add_path -P -p /run/setuid-programs"
                                      "fish_add_path -P -p ~/.local/bin"
                                      "zoxide init fish | source"
                                      "export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)"
                                      "gpgconf --launch gpg-agent"
                                      "function fish_greeting; FORTUNE_PATH=~/.local/share/fortunes zfortune | cowsay -f small | lolcat;end")
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
  ;; SSH config
  (service home-openssh-service-type
         (home-openssh-configuration
          (hosts
           (list (openssh-host (name "github.com")
                               (user "git")
                               (identity-file "~/.ssh/github"))
                 (openssh-host (name "lexihk")
                               (host-name "lexihk.dimsumlabs.com")
                               (user "azz")
                               (port 2200)
                               (identity-file "~/.ssh/gpg_ssh"))
                 (openssh-host (name "lexipine")
                               (host-name "192.168.0.13")
                               (user "azz")
                               (port 2200)
                               (identity-file "~/.ssh/lexipine"))
                 (openssh-host (name "lexidoo")
                               (host-name "10.38.0.5")
                               (user "sibl")
                               (port 2200)
                               (identity-file "~/.ssh/lexidoo"))))
          (authorized-keys (list
                             (plain-file "lexipine.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHuFK16nVWpdxlZlGWoHwCMDLUtbP9uvJwpi7/Comx9j sibl@sibl")
                             (plain-file "gpg_ssh.pub" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDLhOSwuzj0J74VPkb90Suky1roeMcj7Dt3J6Q9sU22GrWXPDCSIqKSzoXrYcNenRTKvJgw0qRIgTMWUUOCtBoXf8Q1AmbE/fzMa2EDQLtmylaOcOk7a6BhJmN2QjfwkDxb2t6BZSM7G1eOl2iOtWWY3F5QxbGU4locZYjeF16xOG+FOyDuvJldsJsAx/5lUcQvZfuwVACIpqxkto8Qab7qWLb4Qzj81fYtoGrTfdl5FuwAkVFsMlDLHSQzPmOrSMc4lRdcqyn5AqMl/hpzTKXP+1pqHOab5tykAO28//hG7I0n7JBC9J53CPrHFyUYLJznJQ4zIj3QjH4LvrCLVgaKI6rziuhurFFPDG47hchIXXysb4XWElMVD+nVu9y92nM9s6/Np47nsS1yn3jVmE2WLinGMk9z7sme4C05lepVW5sW77dG+hlv0OC4p0MPH1WN27LVwnYLWXQp0kElURMpBi4516RYNrQJ3hrdDlBXJj+pfocbTj5HvMqQph3S3OIazAqZ03dzlpzSwjVEU0eASjz6s/V0IvzMUEsJ+yHyCpimxUm1zRQ1hh6REnTCDLK4YPug/M9LHxK3aOzTFDNwZA1ot+NHjfiDmA26Myhrl74GjjRqT0AxLJ22kozVILtuf/0Ez6PpozzA4JLES7GZ9BngLPqWeAxrvuMLMJC5rw== openpgp:0x99DB3DBD")))))
  ;; flatpak setup
  (simple-service 'flatpak-service
                      home-shell-profile-service-type
                      (list (local-file
                              (string-append (getenv "HOME") "/.guix-profile/etc/profile.d/flatpak.sh")
                              "flatpak.sh")))

  ;; copy dotfiles as symlinks
  (service home-dotfiles-service-type
           (home-dotfiles-configuration
            (directories (list "dotfiles"))))
  ;; shepherd services
  (service home-shepherd-service-type
           (home-shepherd-configuration
            (services %my-shepherd-services)))
  %base-home-services)))

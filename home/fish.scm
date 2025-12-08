(define-module (home fish)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-fish-config))

(define home-fish-config
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
               ("PERL5LIB" . "$HOME/perl5/lib/perl5:./local/lib/perl5:$PERL5LIB")
               ("FONTCONFIG_PATH" . "$HOME/.guix-profile/etc/fonts")
               ("EDITOR"         . "nvim")))
            (aliases
             '(("cat" . "bat")
               ("ls"  . "lsd")
               ("emacst" . "emacsclient -t")
               ("em" . "emacsclient -t")
               ("tb" . "ncat termbin.com 9999")
               ("praku" . "podman run --name raku -ti --sig-proxy=true --network=host --rm -v \"$(pwd):/script\" -w /script rakudo-star raku")
               ("6d" . "RAKUDOC=~/Documents/git/doc rakudoc -D")
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
                                      "fish_add_path -P -a ~/.raku/bin"
                                      "fish_add_path -P -a ~/go/bin"
                                      "fish_add_path -P -a ~/Documents/git/star/bin"
                                      "fish_add_path -P -a ~/Documents/git/star/share/perl6/site/bin"
                                      "fish_add_path -P -a ~/Documents/git/star/share/perl6/vendor/bin"
                                      "fish_add_path -P -a ~/Documents/git/star/share/perl6/core/bin"
                                      "fish_add_path -P -p /run/setuid-programs"
                                      "fish_add_path -P -p ~/.local/bin"
                                      "zoxide init fish | source"
                                      "export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)"
                                      "gpgconf --launch gpg-agent"
                                      "function fish_greeting; FORTUNE_PATH=~/.local/share/fortunes zfortune | cowsay -f small | lolcat;end"
                                      "function fish_prompt
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
    set -l prompt_status (__fish_print_pipestatus '[' ']' '|' (set_color red) (set_color red) $last_pipestatus)
    echo -s (set_color purple)'# '(set_color blue)$USER(set_color normal)' @ '(set_color green)$hostname(set_color normal)' in '(set_color yellow)(prompt_pwd --full-length-dirs 2)(set_color normal) (fish_vcs_prompt) ' ' $prompt_status
    echo -n (set_color red)'$ '(set_color normal)
end"
                                    "fish_config theme choose None")
                                "\n")))))))

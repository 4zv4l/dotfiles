# history setup
HISTFILESIZE=50000
HISTSIZE=$HISTFILESIZE
shopt -s histappend
export HISTCONTROL=ignorespace:ignoredups:erasedups

# setup gpg
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpg-connect-agent updatestartuptty /bye > /dev/null
gpgconf --launch gpg-agent

# setup zoxide
eval "$(zoxide init bash)"

# setup perl
eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
PERL5LIB="$PERL5LIB:./local/lib/perl5"

# setup path
PATHS="$HOME/.local/bin
$HOME/.nimble/bin
$HOME/perl5/bin
$HOME/.deno/bin
$HOME/.cargo/bin
$HOME/.rvm/bin
/usr/local/bin
/usr/local/sbin
/usr/bin
/usr/sbin
/bin
/sbin"
PATH="$(echo -n "$PATHS" | tr "\n" ":")"

# setup guix
GUIX_PROFILE="$HOME/.guix-profile"
export PATH="$GUIX_PROFILE/bin:$HOME/.config/guix/current/bin:$PATH"

# setup export
export EDITOR=nvim
TERMINFO_DIRS="$TERMINFO_DIRS:$HOME/.local/share/terminfo"

# setup prompt
export PS1="$(history -a;history -n)$PS1"

# setup alias
alias cat=bat
alias ls=lsd
alias v=nvim
alias open=xdg-open
alias nu='clear;nu'

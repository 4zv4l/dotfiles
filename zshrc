# exports
export PATH="$HOME/.local/bin:$PATH"
export EDITOR='nvim'
export ZSH="$HOME/.oh-my-zsh"

# theme
ZSH_THEME="minimal"

# brew autocomplete
FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

plugins=(
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh


## setup zoxide
eval "$(zoxide init zsh)"
## setup plenv
eval "$(plenv init -)"
#eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"
## setup gpg-agent for ssh
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent
export GPG_TTY=$(tty)

## Functions
function yy() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

## Prompt
fpath+=("$(brew --prefix)/share/zsh/site-functions")
autoload -U promptinit; promptinit
prompt pure

# aliases
alias v=nvim
alias cat=bat
alias ls='lsd --icon never'
alias xclip=pbcopy

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# PATH
export PATH=$PATH:~/go/bin
export PATH=$PATH:/home/azz/.local/share/gem/ruby/3.0.0/bin
export PATH=$PATH:/home/azz/.nimble/bin
export PATH=$PATH:/home/azz/zig

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# prompt theme
# ZSH_THEME="mine"
# ZSH_THEME="spaceship"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting zsh-completions)

# idk
export TERM="xterm-256color"
# zoxide
eval "$(zoxide init zsh)"


# ssh-agent
# eval $(ssh-agent -s)


source $ZSH/oh-my-zsh.sh
############################ ALIASES
alias open=xdg-open
alias clea=clear
alias cls=clear
alias ls=lsd
alias cat=bat
#alias bat='bat --style numbers,changes --color=always {} | head 100'
alias gmi='gem install --user-install'
alias zb='zig build'
alias zbb='zig build-exe'
alias zbe='zig build-exe -O ReleaseSmall --strip'
alias zbr='zig build run'
alias zie='zig init-exe'
alias music='ncmpcpp'
alias new='~/script/new'
alias v=nvim
alias vv='~/script/vv'
alias rby='irb --simple-prompt'
alias cht='~/script/cht.sh'
alias py=python3
alias ydl='~/script/add_music'
alias xclip='xclip -selection clipboard'
alias doom='gzdoom -iwad ~/Games/Doom/wad/freedoom1.wad -file ~/Games/Doom/mods/brutalv21.pk3'
alias wordle='wordle $WORDLE'
alias myinfo='glow $HOME/Documents/info.md'
alias cwd='echo ${PWD##*/}'
alias cam='mplayer tv://device=/dev/video1 -vf mirror'
alias splitMK="echo csplit --prefix='commandline' --suffix-format='%03d.md'  CommandLine.md /##/ \"{*}\""
alias tmpnote='~/script/tmpnote'
############################ VAR
export GIT="$HOME/Documents/git"
export CODE="$GIT/code"
export GO="$GIT/code/Go"
export C="$GIT/code/C"
export RUST="$GIT/code/rust"
export PY="$GIT/code/py"
export RUBY="$GIT/code/ruby"
export BOOKS="$HOME/Documents/pdf"
export DOOM="$HOME/.config/gzdoom"
export WORDLE="$HOME/Documents/git/wordle/words.txt"
export HENA="$HOME/Documents/git/hena/SecondYear/Q2"
export LIBRARY_PATH='/usr/local/lib'
export C_INCLUDE_PATH='/usr/local/include'
export ANIME="$HOME/Videos/anime"
############################ PROMPT
source "$HOME/.oh-my-zsh/custom/themes/mine.zsh-theme"
setopt promptsubst
NEWLINE=$'\n'
export PS1='$(prompt)${NEWLINE}%F{102}╚%f %F{23}$%f '

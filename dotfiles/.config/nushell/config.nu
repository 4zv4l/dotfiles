# basic setup
$env.PATH = [
    "/home/sibl/.local/bin"
    "/home/sibl/.nimble/bin"
    "/home/sibl/perl5/bin"
    "/home/sibl/.deno/bin"
    "/home/sibl/.cargo/bin"
    "/home/sibl/.rvm/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/usr/bin"
    "/usr/sbin"
    "/bin"
    "/sbin"
]
$env.config.show_banner = false
$env.config.buffer_editor = "nvim"

# setup zoxide
source ~/.zoxide.nu

# alias
alias v = nvim
alias cat = bat

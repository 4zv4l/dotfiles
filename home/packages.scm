(define-module (home packages)
  #:use-module (gnu packages)
  #:export (home-packages))

(define home-packages
  (specifications->packages
   (list 
    ;; cli utils
    "bat"
    "cowsay"
    "fish"
    "fzf"
    "lolcat"
    "lsd"
    "pigz"
    "pv"
    "rsync"
    "tmux"
    "tree"
    "unzip"
    "vlc"
    "zfortune"
    "zip"
    "zoxide"
    "zstd"
    ;; services
    "syncthing"
    ;; networking
    "mtr"
    "nmap"
    ;; coding
    "go"
    "gopls"
    "gcc-toolchain"
    "guile"
    "guile-lsp-server"
    "make"
    "nushell"
    "perl"
    "pkg-config"
    "ltrace"
    "strace"
    "zig"
    "zig-zls"
    ;; editor
    "emacs-geiser"
    "emacs-geiser-guile"
    "emacs-pgtk"
    "neovim")))

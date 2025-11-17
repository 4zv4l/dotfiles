(define-module (systems lexipad-packages)
  #:use-module (gnu packages)
  #:export (lexipad-packages))

(define lexipad-packages
  (specifications->packages
   (list
    ;; cli
    "emacs"
    "fish"
    "git"
    "mg"
    "tmux"
    "tree"
    "vim"
    ;; utils
    "7zip"
    "btop"
    "file"
    "htop"
    "libnotify"
    "make"
    "mosh"
    "ncurses"
    "openssl"
    "pigz"
    "pinentry"
    "qemu"
    "tlp"
    "unzip"
    "virt-manager"
    "vlc"
    "zip"
    ;; script
    "guile"
    "perl"
    ;; networking
    "mtr"
    "nmap"
    "bind:utils"
    "wireguard-tools"
    ;; fs
    "btrfs-progs"
    "cryptsetup"
    "flatpak"
    "fuse")))

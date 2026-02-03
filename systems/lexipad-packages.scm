(define-module (systems lexipad-packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)                         ; btop, htop, tree
  #:use-module (gnu packages base)                          ; make, glibc-locales
  #:use-module ((gnu packages compression) #:prefix com:)   ; zip unzip
  #:use-module (gnu packages cryptsetup)                    ; cryptsetup
  #:use-module (gnu packages dns)                           ; bind
  #:use-module (gnu packages text-editors)                  ; mg
  #:use-module (gnu packages emacs)                         ; emacs
  #:use-module (gnu packages file)                          ; file
  #:use-module (gnu packages linux)                         ; btrfs-progs
  #:use-module (gnu packages file-systems)                  ; fuse
  #:use-module (gnu packages fonts)                         ; all fonts
  #:use-module (gnu packages freedesktop)                   ; xdg-utils, portals
  #:use-module (gnu packages gnome)                         ; gnome utils, libnotify
  #:use-module (gnu packages gnupg)                         ; pinentry
  #:use-module (gnu packages ibus)                          ; ibus
  #:use-module (gnu packages ncurses)                       ; ncurses
  #:use-module (gnu packages networking)                    ; mtr, nmap
  #:use-module (gnu packages package-management)            ; flatpak
  #:use-module (gnu packages perl)                          ; perl
  #:use-module (gnu packages shells)                        ; fish
  #:use-module (gnu packages ssh)                           ; mosh
  #:use-module (gnu packages tls)                           ; openssl
  #:use-module (gnu packages tmux)                          ; tmux
  #:use-module (gnu packages tor-browsers)                  ; torbrowser
  #:use-module (gnu packages version-control)               ; git
  #:use-module (gnu packages video)                         ; vlc
  #:use-module (gnu packages vim)                           ; vim
  #:use-module (gnu packages vpn)                           ; wireguard-tools
  #:use-module (gnu packages virtualization)                ; qemu, virt-manager
  #:use-module (gnu packages wine)                          ; wine
  #:use-module (gnu packages wm)                            ; wlr-randr
  #:use-module (gnu packages xdisorg)                       ; wl-clipboard

  #:export (lexipad-packages))

(define lexipad-packages
  (list
    ;; --- Editors & Shells ---
    emacs
    fish
    git
    mg
    tmux
    vim

    ;; --- Desktop Environment ---
    gnome-disk-utility
    gnome-software
    gnome-tweaks
    libnotify
    xdg-utils
    xdg-desktop-portal
    xdg-desktop-portal-gnome
    xdg-desktop-portal-wlr
    wl-clipboard
    wlr-randr

    ;; --- Input Methods ---
    ibus
    ibus-libpinyin
    ibus-rime

    ;; --- Internet & Media ---
    mosh
    torbrowser
    vlc
    wine

    ;; --- Virtualization ---
    flatpak
    qemu
    virt-manager

    ;; --- Networking ---
    (specification->package+output "bind:utils")
    mtr
    nmap
    wireguard-tools
    openssl

    ;; --- System Utilities ---
    btop
    file
    glibc-locales
    htop
    gnu-make
    ncurses
    pinentry
    tree

    ;; --- Filesystem ---
    btrfs-progs
    cryptsetup
    fuse
    com:zstd
    com:pigz
    com:unzip
    com:zip
    
    ;; --- Scripting ---
    perl

    ;; --- Fonts ---
    font-adobe-source-han-sans
    font-arphic-ukai
    font-dejavu
    font-gnu-freefont
    font-gnu-unifont
    font-google-noto
    font-google-noto-emoji
    font-ipa-ex
    font-ipa-mj-mincho
    font-lxgw-heartserif
    font-lxgw-neozhisong
    font-plangothic
    font-space-grotesk
    font-wqy-microhei
    font-wqy-zenhei
  ))

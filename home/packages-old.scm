(define-module (home packages-old)
  ;; Import custom module for zfortune
  #:use-module (sibl zfortune)

  ;; Import standard GNU modules
  #:use-module (gnu packages admin)          ; tree, nmap, mtr
  #:use-module (gnu packages base)           ; make
  #:use-module (gnu packages commencement)   ; gcc-toolchain
  #:use-module (gnu packages compression)    ; pigz, unzip, zip, zstd
  #:use-module (gnu packages emacs)          ; emacs-pgtk
  #:use-module (gnu packages emacs-xyz)      ; emacs-geiser, emacs-geiser-guile
  #:use-module (gnu packages games)          ; cowsay
  #:use-module (gnu packages golang)         ; go
  #:use-module (gnu packages golang-apps)    ; gopls
  #:use-module (gnu packages guile)          ; guile
  #:use-module (gnu packages guile-xyz)      ; guile-lsp-server
  #:use-module (gnu packages linux)          ; ltrace, strace
  #:use-module (gnu packages networking)     ; mtr (often here or admin)
  #:use-module (gnu packages perl)           ; perl
  #:use-module (gnu packages pkg-config)     ; pkg-config
  #:use-module (gnu packages rsync)          ; rsync
  #:use-module (gnu packages rust-apps)      ; bat, lsd, zoxide
  #:use-module (gnu packages shells)         ; fish
  #:use-module (gnu packages nushell)        ; nushell
  #:use-module (gnu packages syncthing)      ; syncthing
  #:use-module (gnu packages terminals)      ; fzf
  #:use-module (gnu packages tmux)           ; tmux
  #:use-module (gnu packages toys)           ; lolcat (sometimes in games)
  #:use-module (gnu packages video)          ; vlc
  #:use-module (gnu packages pv)             ; pv
  #:use-module (gnu packages vim)            ; neovim
  #:use-module (gnu packages zig)            ; zig
  #:use-module (gnu packages zig-xyz)        ; zls

  #:export (home-packages-old))

(define home-packages
  (list
    ;; -- Custom --
    zfortune

    ;; -- CLI Utils --
    bat
    cowsay
    fish
    fzf
    lolcat
    lsd
    pigz
    pv
    rsync
    tmux
    tree
    unzip
    vlc
    zip
    zoxide
    zstd

    ;; -- Services --
    syncthing

    ;; -- Networking --
    mtr
    nmap

    ;; -- Coding --
    go
    gopls
    gcc-toolchain
    guile-lsp-server
    gnu-make
    nushell
    perl
    pkg-config
    ltrace
    strace
    zig
    zig-zls

    ;; -- Editor --
    emacs-geiser
    emacs-geiser-guile
    emacs-pgtk
    neovim))

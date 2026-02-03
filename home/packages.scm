(define-module (home packages)
  ;; -- Custom Library Imports --
  #:use-module (sibl zfortune)
  
  ;; -- Library Imports --
  #:use-module (gnu packages admin)          ; btop, iotop, tree
  #:use-module (gnu packages compression)    ; zip, unzip, zstd, 7zip, pigz
  #:use-module (gnu packages curl)           ; curl
  #:use-module (gnu packages emacs)          ; emacs-pgtk
  #:use-module (gnu packages emacs-xyz)      ; emacs-geiser
  #:use-module (gnu packages games)          ; cowsay
  #:use-module (gnu packages gnupg)          ; pinentry, gnupg
  #:use-module (gnu packages golang)         ; go
  #:use-module (gnu packages golang-apps)    ; gopls
  #:use-module (gnu packages guile)          ; guile
  #:use-module (gnu packages guile-xyz)      ; guile-lsp-server
  #:use-module (gnu packages mail)           ; neomutt, msmtp
  #:use-module (gnu packages networking)     ; mtr, nmap
  #:use-module (gnu packages nushell)        ; nushell
  #:use-module (gnu packages pulseaudio)     ; pavucontrol
  #:use-module (gnu packages pv)             ; pv
  #:use-module (gnu packages rsync)          ; rsync
  #:use-module (gnu packages rust-apps)      ; bat, lsd, zoxide, ripgrep
  #:use-module (gnu packages syncthing)      ; syncthing
  #:use-module (gnu packages terminals)      ; fzf
  #:use-module (gnu packages textutils)      ; tldr
  #:use-module (gnu packages tmux)           ; tmux
  #:use-module (gnu packages toys)           ; lolcat
  #:use-module (gnu packages version-control); git, jujutsu
  #:use-module (gnu packages video)          ; yt-dlp
  #:use-module (gnu packages vim)            ; neovim
  #:use-module (gnu packages web)            ; jq
  #:use-module (gnu packages zig)            ; zig
  #:use-module (gnu packages zig-xyz)        ; zls

  #:export (home-packages))

(define home-packages
  (list
    ;; --- Coding & Compilers ---
    go
    gopls            
    guile-3.0
    guile-lsp-server
    zig
    zig-zls          

    ;; --- Editors ---
    emacs-pgtk
    emacs-geiser
    emacs-geiser-guile
    neovim
    jujutsu      

    ;; --- CLI Power Tools ---
    bat
    btop
    curl
    cowsay
    gnupg
    fzf
    jq
    lolcat
    lsd
    neofetch
    nushell
    pavucontrol
    pinentry
    pv
    ripgrep
    rsync
    tldr
    tmux
    tree
    yt-dlp
    zoxide
    
    ;; --- Networking ---
    mtr          
    nmap

    ;; --- Archive Tools ---
    7zip
    pigz
    unzip
    zip
    zstd

    ;; --- Services ---
    syncthing
    neomutt
    
    ;; --- Custom ---
    zfortune
  ))

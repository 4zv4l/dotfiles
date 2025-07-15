(use-modules (gnu)
             (gnu packages shells)
             (gnu system accounts)
             (gnu services containers)
             (gnu services docker)
             (nongnu packages linux)
             (nongnu system linux-initrd))
(use-service-modules cups desktop networking ssh xorg)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_HK.utf8")
 (timezone "Asia/Hong_Kong")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "lexidoo")

 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "sibl")
                (comment "Simon")
                (group "users")
                (home-directory "/home/sibl")
                (shell (file-append fish "/bin/fish"))
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 ;; Packages installed system-wide.  Users can also install packages
 ;; under their own account: use 'guix search KEYWORD' to search
 ;; for packages and 'guix install PACKAGE' to install a package.
 (packages (append (list
                    (specification->package "fish")
                    (specification->package "vim")
                    (specification->package "emacs")
                    (specification->package "mg")
                   ; (specification->package "zstd")
                    (specification->package "pigz")
                    (specification->package "zip")
                    (specification->package "unzip")
                    (specification->package "nmap")
                    (specification->package "mtr")
                    (specification->package "htop")
                    (specification->package "guile")
                    (specification->package "perl")
                    (specification->package "zig")
                    (specification->package "mosh")
                    (specification->package "ncurses")
                    (specification->package "git")
                    (specification->package "tmux"))
                   %base-packages))

 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append (list (service gnome-desktop-service-type)
                (service iptables-service-type)
                (service rootless-podman-service-type
                         (rootless-podman-configuration
                          (subgids
                           (list (subid-range (name "sibl"))))
                          (subuids
                           (list (subid-range (name "sibl"))))))

                ;; docker
                ;;(service containerd-service-type)
                ;;(service docker-service-type)

                ;; To configure OpenSSH, pass an 'openssh-configuration'
                ;; record as a second argument to 'service' below.
                (service openssh-service-type
                         (openssh-configuration
                          (port-number 2200)
                          (permit-root-login #f)
                          (x11-forwarding? #t)
                          (password-authentication? #f)
                          (public-key-authentication? #t)
                          (authorized-keys
                            `(("sibl" ,(plain-file "authorized-keys" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB8ue6dlphwDYWqNJhjmX9FzbvDjw+IGZd+hAlcBSAMs sibl@sibl"))))))
                (set-xorg-configuration
                 (xorg-configuration (keyboard-layout keyboard-layout))))
          ;; This is the default list of services we
          ;; are appending to.
          (modify-services %desktop-services
                           (guix-service-type config => (guix-configuration
                                                         (inherit config)
                                                         (substitute-urls
                                                          ;(append (list "https://substitutes.nonguix.org")
                                                          ;       %default-substitute-urls))
                                                          (append (list "https://nonguix-proxy.ditigal.xyz")
                                                                  %default-substitute-urls))
                                                         (authorized-keys
                                                          (append (list
                                                                    (plain-file "non-guix.pub" "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                                                  %default-authorized-guix-keys)))))))
    (bootloader 
        (bootloader-configuration
            (bootloader grub-efi-removable-bootloader)
            (targets '("/boot/efi"))))
    (file-systems 
        %base-file-systems))  ;; Include the default essential file systems

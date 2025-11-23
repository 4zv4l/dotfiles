(define-module (systems lexidoo)
  #:use-module (gnu)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages shells)
  #:use-module (gnu services containers)
  #:use-module (gnu services mcron)
  #:use-module (gnu services security-token)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (systems base-server))

(operating-system
 (inherit base-server)
 (host-name "lexidoo")
 (keyboard-layout (keyboard-layout "us"))

 (users (cons* (user-account
                (name "sibl")
                (comment "Simon")
                (group "users")
                (home-directory "/home/sibl")
                (shell (file-append fish "/bin/fish"))
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 (packages (append (list
                    (specification->package "vim")
                    (specification->package "mg")
                    (specification->package "pigz")
                    (specification->package "zip")
                    (specification->package "unzip")
                    (specification->package "nmap")
                    (specification->package "mtr")
                    (specification->package "htop")
                    (specification->package "perl")
                    (specification->package "mosh")
                    (specification->package "ncurses")
                    (specification->package "git")
                    (specification->package "gnupg")
                    (specification->package "tmux"))
                   %base-packages))

 (services
  (append (list 
            (service rootless-podman-service-type
			 (rootless-podman-configuration
			  (subgids
			   (list (subid-range (name "sibl"))))
			  (subuids
			   (list (subid-range (name "sibl"))))))

                (service openssh-service-type
                         (openssh-configuration
                           (port-number 2200)
                           (permit-root-login #f)
                           (x11-forwarding? #t)
                           (password-authentication? #f)
                           (public-key-authentication? #t)
                           (authorized-keys
                             `(("sibl" ,(plain-file "lexidoo_ssh" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB8ue6dlphwDYWqNJhjmX9FzbvDjw+IGZd+hAlcBSAMs sibl@sibl")))))))

          %my-base-services))

 (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list "/boot/efi"))
               (keyboard-layout keyboard-layout)))

 (mapped-devices (list (mapped-device
                         (source (uuid "90a2938c-12cd-441e-aacc-a077d120e104"))
                         (target "cryptroot")
                         (type luks-device-mapping))))

 (file-systems (cons* (file-system
                        (mount-point "/boot/efi")
                        (device (uuid "C797-D8C5" 'fat32))
                        (type "vfat"))
                      (file-system
                        (mount-point "/")
                        (device "/dev/mapper/cryptroot")
                        (type "btrfs")
                        (dependencies mapped-devices))
                      %base-file-systems)))

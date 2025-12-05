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
  #:use-module (systems base-desktop))

(operating-system
 (inherit base-desktop)
 (host-name "lexidoo")
 (keyboard-layout (keyboard-layout "us"))

 (users (cons* (user-account
                (name "ande")
                (comment "Ande")
                (group "users")
                (home-directory "/home/ande")
                (supplementary-groups '("netdev" "audio" "video")))
               (user-account
                (name "sibl")
                (comment "Simon")
                (group "users")
                (home-directory "/home/sibl")
                (shell (file-append fish "/bin/fish"))
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 (packages (append (list
                    (specification->package "vim")
                    (specification->package "emacs")
                    (specification->package "mg")
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
                    (specification->package "gnupg")
                    (specification->package "tmux"))
                   %base-packages))

 (services
  (append (list 
            (service rootless-podman-service-type
			 (rootless-podman-configuration
			  (subgids
			   (list (subid-range (name "ande"))
				     (subid-range (name "sibl"))))
			  (subuids
			   (list (subid-range (name "ande"))
				     (subid-range (name "sibl"))))))

                (service openssh-service-type
                         (openssh-configuration
                           (port-number 2200)
                           (permit-root-login #f)
                           (x11-forwarding? #t)
                           (password-authentication? #f)
                           (public-key-authentication? #t)
                           (authorized-keys
                             `(("sibl" ,(plain-file "lexidoo_ssh" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDLhOSwuzj0J74VPkb90Suky1roeMcj7Dt3J6Q9sU22GrWXPDCSIqKSzoXrYcNenRTKvJgw0qRIgTMWUUOCtBoXf8Q1AmbE/fzMa2EDQLtmylaOcOk7a6BhJmN2QjfwkDxb2t6BZSM7G1eOl2iOtWWY3F5QxbGU4locZYjeF16xOG+FOyDuvJldsJsAx/5lUcQvZfuwVACIpqxkto8Qab7qWLb4Qzj81fYtoGrTfdl5FuwAkVFsMlDLHSQzPmOrSMc4lRdcqyn5AqMl/hpzTKXP+1pqHOab5tykAO28//hG7I0n7JBC9J53CPrHFyUYLJznJQ4zIj3QjH4LvrCLVgaKI6rziuhurFFPDG47hchIXXysb4XWElMVD+nVu9y92nM9s6/Np47nsS1yn3jVmE2WLinGMk9z7sme4C05lepVW5sW77dG+hlv0OC4p0MPH1WN27LVwnYLWXQp0kElURMpBi4516RYNrQJ3hrdDlBXJj+pfocbTj5HvMqQph3S3OIazAqZ03dzlpzSwjVEU0eASjz6s/V0IvzMUEsJ+yHyCpimxUm1zRQ1hh6REnTCDLK4YPug/M9LHxK3aOzTFDNwZA1ot+NHjfiDmA26Myhrl74GjjRqT0AxLJ22kozVILtuf/0Ez6PpozzA4JLES7GZ9BngLPqWeAxrvuMLMJC5rw== sibl@sibl"))
                               ("ande" ,(plain-file "lexidoo_ssh" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIUDDToHCKFCuj5JdknEdq+HI/J/Kk3ZivLXXqn5rSCr ande@odoo.com"))))))

                (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

          %my-base-desktop-services))

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
                        (type "ext4")
                        (dependencies mapped-devices))
                      %base-file-systems)))

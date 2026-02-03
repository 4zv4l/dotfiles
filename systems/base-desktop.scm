(define-module (systems base-desktop)
  #:use-module (gnu)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages shells)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system shadow)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:export (base-desktop %my-base-desktop-services))

(define %my-channels
  (list (channel
         (name 'sibl)
         (url "https://github.com/4zv4l/sibl-channel")
         (branch "main")
         (introduction
          (make-channel-introduction
           "749146d77482a79c5cbfc8c6786f8edc670fa2af"
           (openpgp-fingerprint
            "F656 64BF 2D60 DA21 095F  CA50 7FD2 BABC CDDE 5BF1"))))
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (branch "master")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
        (channel
         (name 'guix)
         (url "https://codeberg.org/guix/guix.git")
         (branch "master")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(define %my-base-desktop-services
  (append
   (list (service gnome-desktop-service-type)
         (service bluetooth-service-type)
         (service iptables-service-type)

         (service cups-service-type
                  (cups-configuration
                   (web-interface? #t)
                   (extensions
                    (list cups-filters epson-inkjet-printer-escpr hplip-minimal brlaser)))))

   (modify-services %desktop-services
                    (guix-service-type
                     config => (guix-configuration
                                (inherit config)
                                (channels %my-channels)
                                (guix (guix-for-channels %my-channels))
                                (substitute-urls
                                 (append (list "https://nonguix-proxy.ditigal.xyz" "https://substitutes.nonguix.org")
                                         %default-substitute-urls))
                                (authorized-keys
                                 (append (list (local-file "./nonguix-signing-key.pub"))
                                         %default-authorized-guix-keys)))))))

(define base-desktop
  (operating-system
   (kernel linux)
   (firmware (list linux-firmware))
   (locale "en_HK.utf8")
   (timezone "Asia/Hong_Kong")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "base-desktop")

   (users (cons* (user-account
                  (name "sibl")
                  (comment "sibl")
                  (group "users")
                  (home-directory "/home/sibl")
                  (shell (file-append fish "/bin/fish"))
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                 %base-user-accounts))

   (services %my-base-desktop-services)

   (bootloader 
    (bootloader-configuration
     (bootloader grub-efi-removable-bootloader)
     (targets '("/boot/efi"))))

   (file-systems
    (cons
     (file-system
      (mount-point "/")
      (device "/dev/vda1")
      (type "ext4"))
     %base-file-systems))))

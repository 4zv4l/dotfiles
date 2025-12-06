(define-module (systems base-server)
  #:use-module (gnu)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages shells)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system shadow)
  #:use-module (gnu services networking)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:export (base-server %my-base-services))

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
         (url "https://codeberg.org/guix/guix-mirror")
         (branch "master")
         (introduction
          (make-channel-introduction
           "287aec56a53fbd66492711c375d0b39ccb6c1590"
           (openpgp-fingerprint
            "6980 A9B9 5202 AA11 EB1D  8922 8499 AC88 F1A7 1CF2"))))))

(define %my-base-services
  (append
   (list (service iptables-service-type))

   (modify-services %base-services
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

(define base-server
  (operating-system
   (kernel linux)
   (firmware (list linux-firmware))
   (locale "en_HK.utf8")
   (timezone "Asia/Hong_Kong")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "lexibase")

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

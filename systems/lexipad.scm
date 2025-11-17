(define-module (systems lexipad)
  #:use-module (gnu)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages shells)
  #:use-module (gnu services containers)
  #:use-module (gnu services mcron)
  #:use-module (gnu services security-token)
  #:use-module (gnu services virtualization)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (systems base-desktop)
  #:use-module (systems lexipad-packages))

(operating-system
 (inherit base-desktop)
 (host-name "lexipad")

 (users (cons* (user-account
                (name "sibl")
                (comment "sibl")
                (group "users")
                (home-directory "/home/sibl")
                (shell (file-append fish "/bin/fish"))
                (supplementary-groups '("wheel" "netdev" "audio" "video" "plugdev" "libvirt" "lp" "lpadmin")))
               %base-user-accounts))

 (packages (append lexipad-packages %base-packages))

 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append (list 
                                        ; Yubikey
           (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
           (service pcscd-service-type)

           (service rootless-podman-service-type
                    (rootless-podman-configuration
                     (subgids
                      (list (subid-range (name "sibl"))))
                     (subuids
                      (list (subid-range (name "sibl"))))))

           (service libvirt-service-type
                    (libvirt-configuration
                     (listen-tls? #f)))
           (service virtlog-service-type
                    (virtlog-configuration))

           (simple-service 'crons
                           mcron-service-type
                           (list
                            #~(job
                               '(next-hour '(9 10 11 12 15 16 17 18 22)) ;; will try multi times a day
                               ;;              target tag   keep time between snap (24h)
                               "/home/sibl/.local/bin/btrfs-snp /home  home_daily 3    86400 /.snapshots"
                               "take a btrfs snapshot of /home everyday, keeps 3 days."))))

          ;; used by gdm
          (set-xorg-configuration
           (xorg-configuration (keyboard-layout keyboard-layout))))

  %my-base-desktop-services))

(mapped-devices
 (list (mapped-device
        (source (uuid "a8a7265f-0274-4af1-a986-3ea348182bde"))
        (target "rgcrypt")
        (type luks-device-mapping))))

(file-systems
 (append
  (map (lambda (item)
         (file-system
          (device "/dev/mapper/rgcrypt")
          (mount-point (car item))
          (type "btrfs")
          (flags (list 'no-atime))
          (options (car (cdr item)))
          (dependencies mapped-devices)))
       '(("/"               "compress=zstd,subvol=@")
         ("/home"           "compress=zstd,subvol=@home")
         ("/gnu"            "compress=zstd,subvol=@gnu")
         ("/.snapshots"     "compress=zstd,subvol=@snapshots")
         ("/swap"           "subvol=@swap")))

  (list
   (file-system
    (mount-point "/boot/efi")
    (device
     (uuid "5239-9D1C" 'fat32))
    (type "vfat")))
  %base-file-systems))

(swap-devices
 (list
  (swap-space
   (target "/swap/swapfile")
   (dependencies file-systems)))))

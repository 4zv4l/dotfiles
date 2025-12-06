(define-module (home lexihk-home)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (home fish)
  #:use-module (home packages)
  #:use-module (home services emacs)
  #:use-module (home services syncthing)
  #:use-module (home ssh)
  #:use-module (ice-9 string-fun))

(home-environment
 (packages home-packages)

 (services
  (cons*

   ;; FISH config
   home-fish-config

   ;; SSH config
   home-ssh-config

   ;; copy dotfiles as symlinks
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories (list "dotfiles"))))

   ;; shepherd services
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (services 
              (list syncthing-service emacs-service))))

   %base-home-services)))

(define-module (home services syncthing)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:export (syncthing-service))

(define syncthing-service
  (shepherd-service
   (documentation "Run Syncthing.")
   (provision '(syncthing))
   (start #~(make-forkexec-constructor
             '("syncthing" "-no-browser")
             #:log-file (string-append (getenv "HOME")
                                       "/log/syncthing.log")))
   (stop #~(make-kill-destructor))))

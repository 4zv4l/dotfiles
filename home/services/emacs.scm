(define-module (home services emacs)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:export (emacs-service))

(define emacs-service
  (shepherd-service
   (documentation "Run the Emacs daemon")
   (provision '(emacs))
   (start #~(make-forkexec-constructor
             '("emacs" "--fg-daemon" "--debug-init")
             ;;#:environment-variables '("GDK_BACKEND=wayland")
             #:log-file (string-append (getenv "HOME")
                                       "/log/emacs.log")))
   (stop #~(make-kill-destructor))))

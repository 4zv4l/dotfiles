(define-module (guix-home-config)
  #:use-module (gnu packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu system shadow))

(home-environment

  (packages (list
    (specification->package "tmux")
    (specification->package "neovim")
    (specification->package "perl")
    (specification->package "python")
    (specification->package "zig")
    (specification->package "mtr")
    (specification->package "nmap")
    (specification->package "sqlite")
    (specification->package "strace")))

  (services
    (append
      (list
        (service home-bash-service-type)

	(service home-dotfiles-service-type
	  (home-dotfiles-configuration
	    (directories (list "./dotfiles")))))

      %base-home-services)))

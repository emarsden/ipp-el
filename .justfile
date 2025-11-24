# For use with the just command runner, https://just.systems/

default:
  @just --list



export INSTALL_EL := '''
    (message "Executing emacs-init")
    (unless (package-installed-p 'plz)
       (package-install 'plz))
    (unless (package-installed-p 'ipp)
       (package-vc-install "https://github.com/emarsden/ipp-el" nil nil 'ipp))

    (require 'ipp)
'''
tmpdir := `mktemp -d`
init-el := tmpdir / "init.el"

# Check whether our package-vc-install instructions work on a pristine install.
installability:
   printf '%s' "$INSTALL_EL" > {{ init-el }}
   ls -l {{ init-el }}
   cat {{ init-el }}
   podman run --rm -ti -v {{ tmpdir }}:/tmp \
     -e TERM=xterm-256color \
     --network=host \
     docker.io/silex/emacs:30.2-ci \
      ${EMACS:-emacs} -l /tmp/init.el


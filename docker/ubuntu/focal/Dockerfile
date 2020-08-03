FROM ocaml/opam2:ubuntu-20.04

WORKDIR /home/opam

RUN sudo apt-get update  \
 && opam switch 4.09 \
 && eval "$(opam env)" \
 && opam remote set-url default https://opam.ocaml.org \
 && opam update \
 && opam depext bap --yes -j 2 \
 && git clone https://github.com/ivg/bap --single-branch --branch=rectifies-base-address-computation \
 && opam pin add bap bap -n \
 && opam install bap --deps-only --yes -j 2 \
 && opam install bap --yes -j 2 \
 && opam clean -acrs \
 && rm -rf /home/opam/.opam/4.0[2-8] \
 && rm -rf /home/opam/.opam/4.10 \
 && rm -rf /home/opam/.opam/4.09/.opam-switch/sources/* \
 && rm -rf /home/opam/opam-repository

ENTRYPOINT ["opam", "config", "exec", "--"]

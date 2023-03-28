{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-21.05-small.tar.gz) { }
, run ? "bash"
}:
(pkgs.buildFHSUserEnv {
  name = "erlymock-env";
  targetPkgs = p:
    with p;
    [
      erlang_nox
      rebar3
      gnumake
      gcc
      coreutils
      vim
      which
    ];
  runScript = ''
    ${run}
  '';
  profile = ''
    export XDG_CONFIG_HOME=/tmp/erlymock-config
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    export GIT_SSL_CAINFO="$SSL_CERT_FILE"
    export LANG=C.UTF-8
  '';

}).env


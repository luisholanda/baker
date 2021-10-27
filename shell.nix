{ pkgs ? import <nixpkgs> {
    overlays = [
      (import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/41b11431e8dfa23263913bb96b5ef1913e01dfc1.tar.gz"))
    ];
  }
}:
let
  inherit (pkgs) stdenv lib;

  rust = pkgs.rust-bin.nightly.latest.default;

  shellHooks = {
    setup-rustfilt = ''
      export PATH=~/.cargo/bin:$PATH
      if ! command -v rustfilt &>/dev/null; then
        echo "Couldn't found rustfilt, installing directly from cargo..."
        cargo install rustfilt
      fi
    '';
  };
in pkgs.mkShell {
  name = "baker";

  buildInputs = [ pkgs.pkgconfig ];

  nativeBuildInputs = [
    rust
    pkgs.gnumake
    pkgs.openssl.dev
    pkgs.protobuf
    pkgs.postgresql.lib
  ];

  shellHook = lib.concatStrings (lib.attrValues shellHooks);
}

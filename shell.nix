# with import <nixpkgs> {};

# stdenv.mkDerivation {
#   name = "dev-environment-psqlREST";

#   buildInputs = with pkgs;  [
#     haskellPackages.postgrest
#   ];
# }

# haskellPackages = pkgs.haskellPackages.override {
#           overrides = self: super: {
#             ghcide = self.callHackageDirect {...} {
#               haskell-lsp-types = self.haskell-lsp-types_0_19_0_0;
#             };
#             haskell-lsp-types_0_19_0_0 = (self.callHackageDirect {...} {});
#             ...
#           };
#         };



{ pkgs ? import <nixpkgs> {} }:
let
  postgrest = (import (fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/b27a19d5bf799f581a8afc2b554f178e58c1f524.tar.gz";
    sha256 = "0xl67j7ns9kzk1arr64r4lfiq74nw0awqbv6hnh8njx07rspqhdb";
  }) {}).haskellPackages.postgrest;

  easy-ps = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "d4879bfd2b595d7fbd37da1a7bea5d0361975eb3";
      sha256 = "0kzwg3mwziwx378kvbzhayy65abvk1axi12zvf2f92cs53iridwh";
    }
  ) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [ postgrest easy-ps.purs easy-ps.psc-package-simple easy-ps.spago easy-ps.spago2nix pkgs.cacert ];
  shellHook = ''
    export PATH="${postgrest}/bin:$PATH"
  '';
}

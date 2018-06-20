{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", haskellPackages ? nixpkgs.pkgs.haskell.packages.${compiler} }:
let
  noCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
  noHaddock = nixpkgs.pkgs.haskell.lib.dontHaddock;
  haskellPackages' = haskellPackages.override {
      overrides = self: super: {
        io-streams = haskellPackages'.callHackage "io-streams" "1.4.0.0" {};
      };
  };
  nano-http = noHaddock (haskellPackages'.callPackage ./nano-http.nix {});
in
  haskellPackages'.callPackage ./firebase-database.nix {
  nano-http = nano-http;
}

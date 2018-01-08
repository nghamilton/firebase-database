{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", haskellPackages ? nixpkgs.pkgs.haskell.packages.${compiler} }:
let
  noCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
  noHaddock = nixpkgs.pkgs.haskell.lib.dontHaddock;
  haskellPackages' = haskellPackages.override {
      overrides = self: super: {
        io-streams = haskellPackages'.callHackage "io-streams" "1.4.0.0" {};
        servant = noCheck (haskellPackages'.callHackage "servant" "0.10" {});
        servant-server = noCheck (haskellPackages'.callHackage "servant-server" "0.10" {});
        servant-client = noCheck (haskellPackages'.callHackage "servant-client" "0.10" {});
      };
  };
  nano-http = noHaddock (haskellPackages'.callPackage ./nano-http.nix {});
  firebase-collegevine = noCheck (noHaddock (haskellPackages'.callPackage ./firebase-collegevine.nix {nano-http=nano-http;}));
in
  haskellPackages'.callPackage ./firebase.nix {
  firebase-collegevine = firebase-collegevine;
  nano-http = nano-http;
}

{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802"}:
let
  # NB: these are only for the dev/test env, and should not be required for building the project 
  inherit (nixpkgs) pkgs;
  #...can't do it like this otherwise overides is undefined for default.nix:
  #haskellPackagesDevTools  = pkgs.haskell.packages.${compiler}.ghcWithPackages extras;
  ghc = pkgs.haskell.packages.${compiler};
  haskellPackagesDevTools = ghc.override {
           overrides = self: super: {
             ghc = ghc.withPackages extras;
             ghcWithPackages = self.ghc.withPackages;
           };
         };

  extras = haskellPackages: with haskellPackages; [
                      # libraries
                      aeson-better-errors ycextra
                      # tools
                      ghc-mod hsdev stylish-haskell hindent codex 
                    ];
in
  (import ./default.nix { inherit nixpkgs compiler; haskellPackages=haskellPackagesDevTools;}).env

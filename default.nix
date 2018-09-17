let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./app.nix { }

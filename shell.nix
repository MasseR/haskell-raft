{ nixpkgs ? import <nixpkgs> {} }:

(nixpkgs.callPackage ./release.nix {}).haskell-raft-shell

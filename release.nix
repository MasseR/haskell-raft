{ nixpkgs ? import <nixpkgs> {} }:

let _nixpkgs = import (nixpkgs.fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "c18759113e82850c75a7248c84aab31dcbb9478e";
  sha256 = "14z7hbx0ylj618amq2rzr1800ai0dy2jwsg8cm7z0ynw8xmibvpf";
}) {};

in

with _nixpkgs;
rec {
  haskell-raft = haskellPackages.callPackage ./default.nix {};
  haskell-raft-shell = buildEnv {
    name = "shell";
    paths = [haskell-raft];
    buildInputs = with haskellPackages; [
      ghcid
      hlint
      stylish-haskell
      cabal-install
      (ghcWithPackages (h: haskell-raft.buildInputs ++ haskell-raft.propagatedBuildInputs))
    ];
  };
}

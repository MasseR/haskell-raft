{ haskellPackages }:

haskellPackages.callCabal2nix "haskell-raft" ./. {}

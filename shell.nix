{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ormolu gmp
  ];
}

# gmp is required to avoid this failure:
# ZZ> stack build
# ai> build (lib + exe)
# Preprocessing library for ai-0.1.0.0..
# Building library for ai-0.1.0.0..
# /nix/store/ffi3j388c39qbbskfqg1zw0x2lmbzhnk-binutils-2.31.1/bin/ld.gold: error: cannot find -lgmp
# collect2: error: ld returned 1 exit status
# `gcc' failed in phase `Linker'. (Exit code: 1)
#
# --  While building package ai-0.1.0.0 using:
#       /home/churlin/.stack/setup-exe-cache/x86_64-linux/Cabal-simple_mPHDZzAJ_2.4.0.1_ghc-8.6.5 --builddir=.stack-work/dist/x86_64-linux/Cabal-2.4.0.1 build lib:ai exe:ai-exe --ghc-options ""
#     Process exited with code: ExitFailure 1

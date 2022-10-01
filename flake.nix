# This file is pretty general, and you can adapt it in your project replacing
# only `name` and `description` below.

{
  description = "ribit";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, utils, rust-overlay, ... }:
    utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ (import rust-overlay) ];
          pkgs = import nixpkgs {
            inherit system overlays;
          };
        in
        rec {
          # `nix develop`
          devShell = pkgs.mkShell
            {
              buildInputs = with pkgs;
                # Tools you need for development go here.
                [
                  (rust-bin.selectLatestNightlyWith
                    (toolchain: toolchain.default.override {
                      extensions = [ "rust-src" ];
                    })
                  )
                  nixpkgs-fmt
                  cargo-outdated
                  cargo-edit
                  cargo-llvm-lines
                ];
            };
        }
      );
}

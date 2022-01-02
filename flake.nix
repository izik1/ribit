# This file is pretty general, and you can adapt it in your project replacing
# only `name` and `description` below.

{
  description = "ribit";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, rust-overlay, ... }:
    let
      # note: we need to build on stable too, just not sure how to actually *have* 
      # nightly available for `cargo +nightly fmt`.
      rustChannel = "nightly";
      rustVersion = "latest";
    in
    utils.lib.eachDefaultSystem
      (system:
        let
          # Imports
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              rust-overlay.overlay
              (self: super: {
                rustc = self.rust-bin.${rustChannel}.${rustVersion}.default;
                cargo = self.rust-bin.${rustChannel}.${rustVersion}.default;
                rustfmt = self.rust-bin.${rustChannel}.${rustVersion}.default;
              })
            ];
          };

          # Configuration for the non-Rust dependencies
          buildInputs = with pkgs; [ ];
          nativeBuildInputs = with pkgs; [ rustc cargo pkgconfig ];
        in
        rec {
          # `nix develop`
          devShell = pkgs.mkShell
            {
              buildInputs = buildInputs ++ (with pkgs;
                # Tools you need for development go here.
                [
                  pkgs.cargo
                  pkgs.rustc
                  pkgs.rustfmt
                  nixpkgs-fmt
                  cargo-outdated
                  cargo-edit
                  cargo-llvm-lines
                ]);
              RUST_SRC_PATH = "${pkgs.rust-bin.${rustChannel}.${rustVersion}.rust-src}/lib/rustlib/src/rust/library";
            };
        }
      );
}

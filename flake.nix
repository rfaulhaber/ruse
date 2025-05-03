{
  description = "ruse";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {flake-parts, ...}: let
    projectName = "ruse";
  in
    flake-parts.lib.mkFlake {inherit inputs;} (top @ {
      config,
      withSystem,
      moduleWithSystem,
      ...
    }: {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
      ];
      perSystem = {
        self',
        config,
        pkgs,
        system,
        ...
      }: {
        formatter = pkgs.alejandra;

        packages.${projectName} = pkgs.rustPlatform.buildRustPackage {
          pname = projectName;
          version = "0.1.0";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;

          meta.mainProgram = projectName;
        };

        packages.default = self'.packages.${projectName};

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cargo
            rustc
            rustfmt
            clippy
            rust-analyzer
            rustup
            cargo-nextest
          ];
        };
      };
    });
}

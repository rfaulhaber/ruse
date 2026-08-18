{
  description = "Rust flake template using rust-overlay and flake-parts.";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }: let
    cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
    projectName = cargoToml.package.name;
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [];
      flake.overlays.rustOverlay = inputs.rust-overlay.overlays.default;
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
      ];

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        # Miri needs nightly. The channel is pinned in nightly-toolchain.toml, which the CI
        # Miri job reads too, so local and CI runs share a compiler.
        miriToolchain = pkgs.rust-bin.fromRustupToolchainFile ./nightly-toolchain.toml;

        # `nix run .#miri` — the collector's unsafe half under both of Miri's aliasing
        # models. Extra arguments are forwarded to `cargo miri test`, so
        # `nix run .#miri -- gc::tests::cycles` filters.
        miri = pkgs.writeShellApplication {
          name = "ruse-miri";
          runtimeInputs = [miriToolchain];
          text = ''
            # NaN-boxing packs a pointer into 48 bits of an integer, which strict provenance
            # cannot express. The code uses expose_provenance/with_exposed_provenance_mut so
            # the exposed-provenance model applies and everything downstream of the cast is
            # still checked; this flag only silences the per-run advisory.
            export MIRIFLAGS="-Zmiri-permissive-provenance ''${MIRIFLAGS:-}"

            cargo miri --version

            echo "==> Miri, Stacked Borrows"
            cargo miri test --lib "$@"

            echo "==> Miri, Tree Borrows"
            MIRIFLAGS="$MIRIFLAGS -Zmiri-tree-borrows" cargo miri test --lib "$@"
          '';
        };
      in {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            self.overlays.rustOverlay
          ];
        };

        formatter = pkgs.alejandra;

        apps.miri = {
          type = "app";
          program = pkgs.lib.getExe miri;
        };

        packages = {
          inherit miri;

          ${projectName} = pkgs.rustPlatform.buildRustPackage {
            pname = projectName;
            version = cargoToml.package.version;
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
          };
          default = self'.packages.${projectName};
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            (rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
            rust-analyzer
            cargo-nextest
            cargo-release
          ];
        };
      };

      flake = {};
    };
}

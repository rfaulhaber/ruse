{
  description = "ruse";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    projectName = "ruse";
    supportedSystems = ["x86_64-linux" "aarch64-darwin" "aarch64-linux"];
    forSystems = systems: f:
      nixpkgs.lib.genAttrs systems
      (system: f system (import nixpkgs {inherit system;}));
    forAllSystems = forSystems supportedSystems;
  in {
    # packages = forAllSystems (system: pkgs: {
    #   ${projectName} = {};
    #   default = self.packages.${system}.${projectName};
    # });

    formatter = forAllSystems (system: pkgs: pkgs.alejandra);

    devShells = forAllSystems (system: pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cargo
          rustc
          rustfmt
          clippy
          rust-analyzer
          rustup
        ];
      };
    });
  };
}

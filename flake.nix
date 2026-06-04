{
  description = "HyperAST";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay?ref=stable";
    nix-filter.url = "github:numtide/nix-filter";
    crane.url = "github:ipetkov/crane";
    nix2container.url = "github:nlewo/nix2container";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      crane,
      rust-overlay,
      nix2container,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        filter = inputs.nix-filter.lib;
        rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
        commonArgs = {
          pname = "HyperAST";
          version = "0.5.0";
          src = filter {
            root = ./.;
            exclude = [
              ./.vscode
              ./.github/workflows
              ./.direnv
              ./target
              ./flake.lock
              ./flake.nix
              ./LICENCES
              ./README.md
            ];
          };
          strictDeps = true;
          OPENSSL_NO_VENDOR = "1";
          doCheck = false;
          buildInputs = with pkgs; [ openssl ];
          nativeBuildInputs = with pkgs; [
            cmake
            pkg-config
          ];
          CARGO_PROFILE_RELEASE_STRIP = "symbols";
          cargoExtraArgs = "-p backend"; # builds both `backend` and `scripting` bins
        };
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        hyperast-backend = craneLib.buildPackage (
          commonArgs
          // {
            inherit cargoArtifacts;
          }
        );
      in
      {
        apps = {
          hyperast-backend = {
            type = "app";
            program = "${hyperast-backend}/bin/backend";
          };
          hyperast-scripting = {
            type = "app";
            program = "${hyperast-backend}/bin/scripting";
          };
        };

        packages = {
          hyperast = hyperast-backend;

          hyperast-dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "HyperAST";
            tag = "0.5.0";
            contents = [
              (pkgs.runCommand "symlinks" { } ''
                mkdir -p $out
                ln -s ${hyperast-backend}/bin/scripting $out/scripting
                ln -s ${hyperast-backend}/bin/backend $out/backend
              '')
            ];
            config = {
              Cmd = [
                "/backend"
                "--"
                "0.0.0.0:8888"
              ];
              Env = [
                "GIT_SSL_CAINFO=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
            };
          };
          hyperast-ociImage = nix2container.packages.${system}.nix2container.buildImage {
            name = "hyperast";
            tag = "0.5.0";

            copyToRoot = [
              (pkgs.runCommand "symlinks" { } ''
                mkdir -p $out
                ln -s ${hyperast-backend}/bin/backend $out/backend
                ln -s ${hyperast-backend}/bin/scripting $out/scripting
              '')
            ];

            config = {
              Cmd = [
                "/backend"
                "--"
                "0.0.0.0:8888"
              ];

              Env = [
                "GIT_SSL_CAINFO=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
            };
          };
        };

        devShell = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            # Rust
            (rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
            trunk

            # misc
            cmake
            pkg-config
            dive
            perl

            # Nix
            nixfmt
          ];
          libraries = with pkgs; [
            # x11 libraries
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            xorg.libX11

            # wayland libraries
            wayland

            # GUI libraries
            libxkbcommon
            libGL
            fontconfig

            # misc libraries
            # openssl
            # libgit2.dev
          ];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libraries;
        };
      }
    );
}

{
  description = "A flake demonstrating how to build OCaml projects with Dune";

  inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        legacyPackages = nixpkgs.legacyPackages.${system};
        ocamlPackages = legacyPackages.ocamlPackages;
        lib = pkgs.lib;

        cplplot = pkgs.callPackage ./Dependencies/nix-pkgs/plplot/default.nix {};

        # Define plplot package
        plplot = ocamlPackages.buildDunePackage rec {
          pname = "plplot";
          version = "5.12.0";
          src = legacyPackages.fetchurl {
            url = "https://github.com/hcarty/ocaml-plplot/releases/download/5.12.0/plplot-5.12.0.tbz";
            sha256 = "fHhsMWJXvy+HBzxEff26xZhOZULzepYmiutFSBUrzCw=";
          };
          buildInputs = [ cplplot ocamlPackages.findlib ocamlPackages.dune-configurator pkgs.cairo ];
          nativeBuildInputs = [ pkgs.pkg-config  ];

          preConfigure = ''
            if [ -d ./configure ]; then
              echo "Configure directory exists, moving it aside."
              mv ./configure ./_configure
            fi
          '';

          preBuild = ''
            if [ -d ./_configure ]; then
              echo "Returning back Configure directory."
              mv ./_configure ./configure
            fi
          '';
          duneConfigFile = "dune-project";
        };

        # Define owl-plplot package
        # owl-plplot = ocamlPackages.buildDunePackage rec {
        #   pname = "owl-plplot";
        #   version = "1.0.2";
        #   src = legacyPackages.fetchurl {
        #     url = "https://github.com/owlbarn/owl/releases/download/1.0.2/owl-1.0.2.tbz";
        #     sha256 = "ONIQzmwcLwljH9WZUUMOTzZLWuA2xx7RsyzlWbKikmM=";
        #   };
        #   buildInputs = [ ocamlPackages.findlib  ocamlPackages.owl plplot ];
        #   nativeBuildInputs = [ pkgs.pkg-config  ];
        #   duneConfigFile = "dune-project";
        # };
        owl-plplot = ocamlPackages.buildDunePackage rec {
          pname = "owl-plplot";
          version = "1.1.0";
          src = legacyPackages.fetchFromGitHub {
            owner = "owlbarn";
            repo = "owl-plplot";
            rev = "ebc73c0";
            sha1= "sha1-T6XyfYI6OETBsJqyP2TkwKjDmy0=";
          };
          buildInputs = [ ocamlPackages.findlib  ocamlPackages.owl plplot ];
          nativeBuildInputs = [ pkgs.pkg-config  ];
          duneConfigFile = "dune-project";
        };

        sources = {
          ocaml = nix-filter.lib {
            root = ./.;
            include = [
              ".ocamlformat"
              "dune-project"
              (nix-filter.lib.inDirectory "bin")
              (nix-filter.lib.inDirectory "lib")
              (nix-filter.lib.inDirectory "test")
            ];
          };

          nix = nix-filter.lib {
            root = ./.;
            include = [
              (nix-filter.lib.matchExt "nix")
            ];
          };
        };
      in
      {
        packages = {
          default = self.packages.${system}.hello;

          hello = ocamlPackages.buildDunePackage {
            pname = "hello";
            version = "0.1.0";
            duneVersion = "3";
            src = sources.ocaml;

            buildInputs = [
              ocamlPackages.owl
            ];

            strictDeps = true;

            preBuild = ''
              dune build hello.opam
            '';
          };
        };

        checks = {
          hello =
            let
              patchDuneCommand =
                let
                  subcmds = [ "build" "test" "runtest" "install" ];
                in
                lib.replaceStrings
                  (lib.lists.map (subcmd: "dune ${subcmd}") subcmds)
                  (lib.lists.map (subcmd: "dune ${subcmd} --display=short") subcmds);
            in

            self.packages.${system}.hello.overrideAttrs
              (oldAttrs: {
                name = "check-${oldAttrs.name}";
                doCheck = true;
                buildPhase = patchDuneCommand oldAttrs.buildPhase;
                checkPhase = patchDuneCommand oldAttrs.checkPhase;
                installPhase = "touch $out";
              });

          dune-fmt = legacyPackages.runCommand "check-dune-fmt"
            {
              nativeBuildInputs = [
                ocamlPackages.dune_3
                ocamlPackages.ocaml
                legacyPackages.ocamlformat
              ];
            }
            ''
              echo "checking dune and ocaml formatting"
              dune build \
                --display=short \
                --no-print-directory \
                --root="${sources.ocaml}" \
                --build-dir="$(pwd)/_build" \
                @fmt
              touch $out
            '';

          dune-doc = legacyPackages.runCommand "check-dune-doc"
            {
              ODOC_WARN_ERROR = "true";
              nativeBuildInputs = [
                ocamlPackages.dune_3
                ocamlPackages.ocaml
                ocamlPackages.odoc
                ocamlPackages.findlib
              ];
            }
            ''
              echo "checking ocaml documentation"
              dune build \
                --display=short \
                --no-print-directory \
                --root="${sources.ocaml}" \
                --build-dir="$(pwd)/_build" \
                @doc
              touch $out
            '';

          nixpkgs-fmt = legacyPackages.runCommand "check-nixpkgs-fmt"
            { nativeBuildInputs = [ legacyPackages.nixpkgs-fmt ]; }
            ''
              echo "checking nix formatting"
              nixpkgs-fmt --check ${sources.nix}
              touch $out
            '';
        };

        devShells = {
          default = legacyPackages.mkShell {
            packages = [
              pkgs.cairo
              legacyPackages.nixpkgs-fmt
              legacyPackages.ocamlformat
              legacyPackages.fswatch
              ocamlPackages.odoc
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat-rpc-lib
              ocamlPackages.utop
              ocamlPackages.base
              ocamlPackages.owl
              owl-plplot
              plplot
            ];

            inputsFrom = [
              self.packages.${system}.hello
            ];
          };
        };
      });
}


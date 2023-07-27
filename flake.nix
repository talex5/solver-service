{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    opam-nix.inputs.opam-repository.follows = "opam-repository";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, opam-repository }@inputs:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};
    on = opam-nix.lib.${system};
    localPackagesQuery = builtins.mapAttrs (_: pkgs.lib.last)
    (on.listRepo (on.makeOpamRepo ./.));
    devPackagesQuery = {
          # You can add "development" packages here. They will get added to the devShell automatically.
          merlin = "*";
          ocp-indent = "*";
        };
        query = devPackagesQuery // {
          ## You can force versions of certain packages here, e.g:
          ## - force the ocaml compiler to be taken from opam-repository:
          ocaml-base-compiler = "*";
          ## - or force the compiler to be taken from nixpkgs and be a certain version:
          # ocaml-system = "4.14.0";
          ## - or force ocamlfind to be a certain version:
          # ocamlfind = "1.9.2";
        };
        scope = on.buildOpamProject' { } ./. query;
        overlay = final: prev:
        {
            # You can add overrides here
          };
          scope' = scope.overrideScope' overlay;
        # Packages from devPackagesQuery
        devPackages = builtins.attrValues
        (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
        # Packages in this workspace
        merlin = 
          (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope').merlin;
        ocp-indent = 
          (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope').ocp-indent;
        packages =
          pkgs.lib.getAttrs (builtins.attrNames localPackagesQuery) scope';
      in {
        legacyPackages = scope';

        inherit packages;

        ## If you want to have a "default" package which will be built with just `nix build`, do this instead of `inherit packages;`:
        # packages = packages // { default = packages.<your default package>; };

        devShells.default = pkgs.mkShell {
          inputsFrom = builtins.attrValues packages;
          shellHook = ''
            export MERLIN=${merlin}
            export OCP_INDENT=${ocp-indent}
          '';
          buildInputs = devPackages ++ [
            # You can add packages from nixpkgs here
          ];
        };
      });
}

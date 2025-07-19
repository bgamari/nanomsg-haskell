{
  description = "nanomsg-haskell";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ nanomsg ];
        };
        packages = rec {
          default = pkgs.haskellPackages.callCabal2nix "nanomsg-haskell" ./. {
            nanomsg = pkgs.nanomsg;
          };
        };
      }
    );
}

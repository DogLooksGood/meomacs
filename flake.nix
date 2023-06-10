{
  description = "Development environment";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              emacs29-nox
              git
              pass
              simple-http-server
              rust-analyzer
              clojure-lsp
              ripgrep
            ];
          };
        }
    );

  nixConfig = {
    bash-prompt-prefix = "[dev]";
  };
}

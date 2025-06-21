{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages =
          with pkgs;
          [
            clang
            gnumake
            zip
          ]
          ++ (with ocaml-ng.ocamlPackages_4_14; [
            dune_2
            menhir
            num
            ocaml
            ocaml-lsp
            ocamlformat_0_26_1
            utop
          ]);
      };

      formatter.${system} = pkgs.nixfmt-rfc-style;
    };
}
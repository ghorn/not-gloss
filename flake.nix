{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=haskell-updates";
    devenv.url = "github:cachix/devenv";
    nix-filter.url = "github:numtide/nix-filter";
    spatial-math.url = "github:smunix/spatial-math?ref=fix.no-TypeCompose";
  };

  outputs = { self, nixpkgs, devenv, nix-filter, ... }@inputs:
    with nix-filter.lib;
    let
      systems = [
        "x86_64-linux"
        # "i686-linux"
        "x86_64-darwin"
        # "aarch64-linux"
        # "aarch64-darwin"
      ];
      config = { };
      forAllSystems = f:
        builtins.listToAttrs (map (name: {
          inherit name;
          value = f name;
        }) systems);
      overlays.default = final: previous:
        with final.haskell.lib; {
          haskellPackages = previous.haskellPackages.extend (hfinal: hprevious:
            with hfinal; {
              not-gloss = callCabal2nix "not-gloss"
                (filter { root = "${self}/not-gloss"; }) { };
              not-gloss-examples = callCabal2nix "not-gloss-examples"
                (filter { root = "${self}/not-gloss-examples"; }) { };
            });
        };
    in {
      inherit overlays;
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit config system;
            overlays =
              [ inputs.spatial-math.overlays.default overlays.default ];
          };
        in {
          default = pkgs.haskellPackages.not-gloss;
          inherit (pkgs.haskellPackages) not-gloss not-gloss-examples;
        });
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit config system;
            overlays =
              [ inputs.spatial-math.overlays.default overlays.default ];
          };
        in {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = with pkgs.haskellPackages;
              with pkgs; [{
                env.name = "not-gloss";
                enterShell = ''
                  setUp
                '';
                packages = [
                  (ghcWithPackages (p:
                    with p; [
                      haskell-language-server
                      not-gloss
                      not-gloss-examples
                    ]))
                ];
                pre-commit.hooks = { nixfmt.enable = true; };
                scripts = {
                  setUp.exec = ''
                    ${implicit-hie}/bin/gen-hie --cabal &> hie.yaml
                  '';
                };
              }];
          };
        });
    };
}

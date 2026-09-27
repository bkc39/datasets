{
  description = "Bundled datasets for Racket: immutable core and dataframe adapters";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/07e1d92cdc0ed416cfa11ff3ca40d17e61cfba7a";
    polars.url = "github:bkc39/rkt-polars/dfe17e1d94356f192fd4a547714324e81b465a7e";
    data-frame = { url = "github:alex-hhh/data-frame/ab3980c4da5a99d2b79172a32b9cb86b2c2b63b4"; flake = false; };
    al2-test-runner = { url = "github:alex-hhh/al2-test-runner/b6757271932151dff6507ee6f1b690d0268da808"; flake = false; };
  };
  outputs = { self, nixpkgs, polars, data-frame, al2-test-runner }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      each = nixpkgs.lib.genAttrs systems;
      # The Racket 9.3 pin no longer supports Intel macOS. Reuse Polars'
      # older pinned platform/toolchain there, with the newer Racket recipes.
      pkgsFor = system:
        if system == "x86_64-darwin" then
          import polars.inputs.nixpkgs {
            inherit system;
            overlays = [ (final: prev: {
              racket-minimal = final.callPackage "${nixpkgs}/pkgs/by-name/ra/racket/minimal.nix" {};
              racket = final.callPackage "${nixpkgs}/pkgs/by-name/ra/racket/package.nix" {};
            }) ];
          }
        else import nixpkgs { inherit system; };
      source = pkgs: pkgs.lib.cleanSourceWith {
        src = ./.;
        filter = path: type: !(builtins.elem (baseNameOf path)
          [ ".git" "compiled" "doc" "result" "__pycache__" ]);
      };
    in {
      packages = each (system:
        let
          pkgs = pkgsFor system;
          native = polars.packages.${system}.rust;
          deps = polars.packages.${system}.racket-deps;
          build = full: pkgs.stdenvNoCC.mkDerivation {
            pname = if full then "datasets" else "datasets-core";
            version = "0.1";
            src = source pkgs;
            nativeBuildInputs = [ pkgs.racket ];
            RKT_POLARS_COMPAT_LIB_PATH = if full then "${native}" else "";
            buildPhase = ''
              runHook preBuild
              export PLTUSERHOME=$TMPDIR/plt
              mkdir -p "$PLTUSERHOME"
              ${pkgs.lib.optionalString full ''
                raco pkg install --batch --deps fail --copy --no-setup --scope user ${deps}/*/
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name al2-test-runner ${al2-test-runner}
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name data-frame ${data-frame}
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name polars ${polars}/polars
                raco setup --no-docs --pkgs tzdata polars data-frame
              ''}
              raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-core ./datasets-core
              ${pkgs.lib.optionalString full ''
                # Exercise downstream integration before the adapter package exists.
                racket examples/core-with-polars.rkt
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets ./datasets
              ''}
              runHook postBuild
            '';
            doCheck = true;
            checkPhase = ''
              runHook preCheck
              bash scripts/check.sh ${if full then "full" else "core"}
              runHook postCheck
            '';
            installPhase = ''
              mkdir -p $out/share
              cp -r "$PLTUSERHOME" $out/share/racket-home
            '';
          };
        in { default = build true; core = build false; inherit native; });
      checks = each (system: {
        inherit (self.packages.${system}) core;
        full = self.packages.${system}.default;
      });
      devShells = each (system:
        let pkgs = pkgsFor system; in {
          default = pkgs.mkShell {
            packages = [ pkgs.racket ];
            RKT_POLARS_COMPAT_LIB_PATH = "${self.packages.${system}.native}";
            shellHook = ''
              export PLTUSERHOME=$(mktemp -d /tmp/datasets-dev.XXXXXXXX)
              cp -R ${self.packages.${system}.default}/share/racket-home/. "$PLTUSERHOME/"
              chmod -R u+w "$PLTUSERHOME"
              raco pkg update --batch --deps fail --no-setup --link --scope user \
                "$PWD/datasets-core" "$PWD/datasets"
              echo "Isolated Racket environment: $PLTUSERHOME"
            '';
          };
          maintainer = pkgs.mkShell { packages = [ pkgs.R pkgs.python3 ]; };
        });
    };
}

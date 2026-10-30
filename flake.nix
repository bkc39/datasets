{
  description = "Bundled datasets for Racket: immutable core and dataframe adapters";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/07e1d92cdc0ed416cfa11ff3ca40d17e61cfba7a";
    polars.url = "github:bkc39/rkt-polars/2b0c382dcac42452be8af14a1a6ab4f085ab67ca";
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
                racket examples/core-with-polars.rkt
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-r ./datasets-r
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-uci ./datasets-uci
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-islr ./datasets-islr
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets-fetch ./datasets-fetch
                raco pkg install --batch --deps fail --copy --no-setup --scope user --name datasets ./datasets
              ''}
              runHook postBuild
            '';
            doCheck = true;
            __darwinAllowLocalNetworking = true;
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
          racket-linters = pkgs.callPackage ./nix/racket-linters.nix { };
          # Dependencies and the pinned Resyntax, independent of this repository's
          # source, so entering the dev shell never rebuilds or re-tests the packages.
          dev-home = pkgs.stdenvNoCC.mkDerivation {
            name = "datasets-dev-home";
            dontUnpack = true;
            dontFixup = true;
            nativeBuildInputs = [ pkgs.racket ];
            RKT_POLARS_COMPAT_LIB_PATH = "${native}";
            buildPhase = ''
              runHook preBuild
              export HOME=$TMPDIR/home
              export PLTUSERHOME=$TMPDIR/plt
              mkdir -p "$PLTUSERHOME"
              raco pkg install --batch --deps fail --copy --no-setup --scope user ${deps}/*/
              raco pkg install --batch --deps fail --copy --no-setup --scope user --name al2-test-runner ${al2-test-runner}
              raco pkg install --batch --deps fail --copy --no-setup --scope user --name data-frame ${data-frame}
              raco pkg install --batch --deps fail --copy --no-setup --scope user --name polars ${polars}/polars
              raco pkg install --batch --deps fail --copy --no-setup --scope user ${racket-linters}/*/
              raco setup -j "$NIX_BUILD_CORES" --pkgs tzdata polars data-frame threading-doc resyntax
              runHook postBuild
            '';
            installPhase = ''
              mkdir -p $out/share
              cp -r "$PLTUSERHOME" $out/share/racket-home
            '';
          };
        in { default = build true; core = build false; inherit native racket-linters dev-home; });
      checks = each (system: {
        inherit (self.packages.${system}) core;
        full = self.packages.${system}.default;
      });
      devShells = each (system:
        let pkgs = pkgsFor system; in {
          default = pkgs.mkShell {
            packages = [ pkgs.racket pkgs.python3 ];
            RKT_POLARS_COMPAT_LIB_PATH = "${self.packages.${system}.native}";
            shellHook = ''
              export PLTUSERHOME=$(mktemp -d /tmp/datasets-dev.XXXXXXXX)
              cp -R ${self.packages.${system}.dev-home}/share/racket-home/. "$PLTUSERHOME/"
              chmod -R u+w "$PLTUSERHOME"
              find "$PLTUSERHOME" -name docindex.sqlite -type f -delete
              raco pkg install --batch --deps fail --no-setup --link --scope user \
                "$PWD/datasets-core" "$PWD/datasets-r" "$PWD/datasets-uci" "$PWD/datasets-islr" "$PWD/datasets-fetch" "$PWD/datasets"
              raco setup --no-docs --pkgs datasets-r datasets-uci datasets-islr datasets-fetch
              echo "Isolated Racket environment: $PLTUSERHOME"
            '';
          };
          maintainer = pkgs.mkShell { packages = [ pkgs.R (pkgs.python3.withPackages (ps: [ ps.xlrd ])) ]; };
        });
    };
}

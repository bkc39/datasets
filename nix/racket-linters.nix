# Resyntax and the packages it needs beyond the Racket distribution, pinned for the dev shell.
# To bump: new rev, hash "", build .#racket-linters, paste Nix's hash (--deps fail names a gap).
{ stdenvNoCC, fetchFromGitHub, fetchFromGitLab, linkFarm, racket }:

let
  github = owner: repo: rev: hash:
    fetchFromGitHub { inherit owner repo rev hash; };
  gitlab = owner: repo: rev: hash:
    fetchFromGitLab { inherit owner repo rev hash; };

  prettyExpressive = github "sorawee" "pretty-expressive"
    "ce4e0e178bf8d492ded88c62a796e44e7f059e70"
    "sha256-Ulzs96eiF47YKO6jZjtWshOyknkn7cmA0UpyeXeDz3s=";

  sources = {
    resyntax = github "jackfirth" "resyntax"
      "40f3497321f8590eb6a0b7c7984a9116323b7ebf"
      "sha256-qd48mLtgWfPiaTN8qzaMxE0Gp48+4OGeXkkaubb/HyQ=";
    br-parser-tools-lib = "${gitlab "mbutterick" "br-parser-tools"
      "95b7c69cf9d660a51abf4742378b9adb7100d25a"
      "sha256-and0y3rBjXwmgaEwwXzJOTgX/wCSY0uUfB3+U4JLTrk="}/br-parser-tools-lib";
    brag-lib = "${gitlab "mbutterick" "brag"
      "30cbf95e6a717e71fb8bda6b15a7253aed36115a"
      "sha256-NJctskWDoBNRdBMDklALkMAPKT4A7on8pu6X3Q6NheE="}/brag-lib";
    fancy-app = github "samth" "fancy-app"
      "f451852164ee67e3e122f25b4bce45001a557045"
      "sha256-2DdngIyocn+CrLf4A4yO9+XJQjIxzKVpmvGiNuM7mTQ=";
    fmt = github "sorawee" "fmt"
      "4e1ed68e596e656960b44a8244bb33eb4e65ec64"
      "sha256-zwcjNvK2qcKfsXb2mlQQNOHI2niMMPI9wx9YeEz+XTo=";
    guard = github "jackfirth" "guard"
      "c86ee57d3fc13eed689102b733702ed1162da681"
      "sha256-Vtcgb9LbaiBkF2ixIwXnQ78rIoou5mYDoy1R6tyz5d8=";
    pretty-expressive = "${prettyExpressive}/pretty-expressive";
    pretty-expressive-lib = "${prettyExpressive}/pretty-expressive-lib";
    rebellion = github "jackfirth" "rebellion"
      "8f3fc46740918205c0a293ad58dd9a8094ef3f51"
      "sha256-xp65LaSuJopJQHuXq22eT7Qe2VL/grbmV9Zxhc+g6UA=";
  };
in
stdenvNoCC.mkDerivation {
  name = "racket-linters";
  dontUnpack = true;
  dontFixup = true;
  nativeBuildInputs = [ racket ];
  buildPhase = ''
    runHook preBuild
    export HOME=$TMPDIR/home
    export PLTUSERHOME=$TMPDIR/plt
    raco pkg install --batch --copy --no-docs --deps fail --scope user \
      -j "$NIX_BUILD_CORES" ${linkFarm "racket-linter-sources" sources}/*/
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r "$PLTUSERHOME"/.local/share/racket/*/pkgs/*/ $out/
    runHook postInstall
  '';
}

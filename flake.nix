{
  description = "Escalating Esqueleto";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              escalating-esqueleto = hfinal.callCabal2nix "escalating-esqueleto" ./. { };
            };
        };
        escalating-esqueleto = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.escalating-esqueleto;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.escalating-esqueleto ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.hlint
              pkgs.bashInteractive
              pkgs.pcre
              (pkgs.haskell-language-server.override { supportedGhcVersions = [ "96" ]; })
              pkgs.postgresql
              pkgs.fswatch
            ];
            shellHook = ''
              export PGDATA=$PWD/.postgres
              export PGPORT=5432
              if [ ! -d "$PGDATA" ]; then
                echo "Initializing PostgreSQL database..."
                initdb -D $PGDATA
              fi
              if ! pg_ctl -D $PGDATA status > /dev/null 2>&1; then
                echo "Starting PostgreSQL..."
                pg_ctl -D $PGDATA -l postgres.log -o "-k /tmp -p $PGPORT" start
              fi
            '';
          };
          defaultPackage = pkgs.escalating-esqueleto;
        };
    in
    { inherit overlay; } //
      inputs.flake-utils.lib.eachDefaultSystem perSystem;
}

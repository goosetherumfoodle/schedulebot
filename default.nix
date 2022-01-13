let
  pkgs = import <nixpkgs> {};
  hsPkgs = pkgs.haskellPackages.override {
    overrides = self: super: {
      twilio = super.twilio.overrideAttrs (old: {
        postPatch = "substituteInPlace twilio.cabal --replace 'containers ==0.5.*' 'containers >=0.5'";
      });
    };
  };
in
hsPkgs.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [
        cabal-install
        ghcid
        hpack
      ]);
}

let
  pkgSrc = fetchTarball "https://github.com/NixOS/nixpkgs/archive/5aaed40d22f0d9376330b6fa413223435ad6fee5.tar.gz";
  pkgs = import pkgSrc {};
  hsPkgs = pkgs.haskellPackages.override {
    overrides = self: super: {
      twilio = super.twilio.overrideAttrs (old: {
        postPatch = "substituteInPlace twilio.cabal --replace 'containers ==0.5.*' 'containers >=0.5'";
        meta.broken = false;
      });
      servant-xml = super.servant-xml.overrideAttrs (old: { meta.broken = false; });
      project-m36 = import (fetchTarball "https://github.com/agentm/project-m36/archive/refs/tags/v0.9.4.tar.gz");
      # streamly = import (fetchTarball "https://github.com/composewell/streamly/releases/download/v0.7.3/streamly-0.7.3.tar.gz");
      xmlbf-xeno = super.xmlbf-xeno.overrideAttrs (old: { meta.broken = false; });
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

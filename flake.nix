{
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/90eadd304c6375f926a0970f87b470e765e7f176";
    # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      haskellExtend = hpFinal: hpPrev: {
        jira-client = hpPrev.callCabal2nix "jira-client" self { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      baseTools = with pkgs; [
        hpack
        cabal-install
        hlint
        tasty-discover
        fourmolu
        weeder
        hsPkgs.doctest
      ];

    in {
      devShells."x86_64-linux".ci = hsPkgs.shellFor {
        packages = p: [ p.butler ];
        buildInputs = baseTools;
      };
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.jira-client ];
        buildInputs = with pkgs;
          [
            ghcid
            haskell-language-server
          ] ++ baseTools;
      };
    };
}

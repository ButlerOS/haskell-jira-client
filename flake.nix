{
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/7a46854f28ab9b99c51353c81d5967f1f6fd9a9b";
    # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      haskellExtend = hpFinal: hpPrev: {
        jira-client = hpPrev.callCabal2nix "jira-client" self { };
        md2jira = hpPrev.callCabal2nix "md2jira" "${self}/md2jira" { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      baseTools = with pkgs; [
        cabal-gild
        hpack
        cabal-install
        hlint
        fourmolu
        weeder
        hsPkgs.doctest
      ];

    in {
      devShells."x86_64-linux".ci = hsPkgs.shellFor {
        packages = p: [ p.jira-client p.md2jira ];
        buildInputs = baseTools;
      };
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.jira-client p.md2jira ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
    };
}

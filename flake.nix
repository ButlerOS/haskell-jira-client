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
      pkg-exe = pkgs.haskell.lib.justStaticExecutables hsPkgs.md2jira;

      baseTools = with pkgs; [
        cabal-gild
        hpack
        cabal-install
        hlint
        fourmolu
        weeder
        hsPkgs.doctest
      ];

      container-name = "ghcr.io/butleros/haskell-jira-client";
      container = pkgs.dockerTools.streamLayeredImage {
        name = container-name;
        tag = "latest";
        created = "now";
        config.Entrypoint = [ "${pkg-exe}/bin/md2jira" ];
        config.WorkingDir = "/data";
        config.Labels = {
          "org.opencontainers.image.source" =
            "https://github.com/ButlerOS/haskell-jira-client";
        };
      };

      publish-container-release = pkgs.writeShellScriptBin "md2jira-release" ''
        set -e
        export PATH=$PATH:${pkgs.gzip}/bin:${pkgs.skopeo}/bin
        IMAGE="docker://${container-name}"

        echo "Logging to registry..."
        echo $GH_TOKEN | skopeo login --username $GH_USERNAME --password-stdin ghcr.io

        echo "Building and publishing the image..."
        ${container} | gzip --fast | skopeo copy docker-archive:/dev/stdin $IMAGE:${pkg-exe.version}

        echo "Tagging latest"
        skopeo copy $IMAGE:${pkg-exe.version} $IMAGE:latest
      '';

    in {
      packages."x86_64-linux".default = pkg-exe;
      # To load the container locally:
      # $(nix build .#container) | gzip --fast | skopeo copy docker-archive:/dev/stdin containers-storage:md2jira:latest
      packages."x86_64-linux".container = container;
      apps."x86_64-linux".publish-container-release = {
        type = "app";
        program = "${publish-container-release}/bin/md2jira-release";
      };

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

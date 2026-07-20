{
  allowUnfree = true;
  packageOverrides =
    pkgs: with pkgs; {
      myPackages = pkgs.buildEnv {
        name = "adr-fw1-r";
        paths = [
          age
          age-plugin-yubikey
          amazon-ecr-credential-helper
          autojump
          awscli2
          bazelisk
          crane
          cowsay
          curl
          discord
          emacs
          evince
          figlet
          fzf
          gcal
          gh
          git
          gomplate
          (google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
          (lib.lowPrio gotools)
          graphviz
          htop
          jq
          krew
          kubectl
          kubectx
          kubelogin-oidc
          kubernetes-helm
          kustomize
          magic-wormhole
          (lib.lowPrio minikube)
          (lib.lowPrio moreutils)
          mosh
          navi
          ncdu
          ngrok
          nmap
          nodejs_26
          ntp
          obsidian
          openssh
          packer
          parallel
          pgcli
          pnpm
          postgresql_16
          pre-commit
          pssh
          pv
          redis
          redshift
          rename
          restic
          ripgrep
          rlwrap
          s6
          shellcheck
          shfmt
          socat
          sox
          spotify
          sqlite-interactive
          starship
          stern
          terraform
          terraform-ls
          tilt
          traceroute
          tree
          v4l-utils
          vault
          vlc
          xclip
          xournalpp
          yamllint
          yubikey-manager
          yq
          zx
        ];
      };
    };
}

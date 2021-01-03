#!/usr/bin/env bash

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

log() {
  echo -n -e '\e[38;5;34m'
  echo -n "$@"
  echo -e '\e[0m'
}

install_keybase() {
  dpkg --verify keybase && return

  log 'installing keybase'
  local pkg_path
  pkg_path="$(mktemp -d)/keybase_amd64.deb"
  curl -o "$pkg_path" https://prerelease.keybase.io/keybase_amd64.deb
  sudo dpkg -i "$pkg_path"
  sudo apt --fix-broken install -y
}

install_intellij() {
  local version install_dir archive_path
  version=$(curl --silent 'https://data.services.jetbrains.com/products/releases?code=IIU&type=release&latest=true' | jq -r '.IIU[0].version')
  install_dir=/opt/intellij/$version
  if [[ ! -e $install_dir ]]; then
    log "installing intellij version $version"
    archive_path=$(mktemp -d)/ideaIU-$version
    curl -L -o "$archive_path" "https://download.jetbrains.com/idea/ideaIU-$version.tar.gz"
    sudo mkdir -p "$install_dir"
    sudo tar --strip-components=1 -C "$install_dir" -xf "$archive_path"
  fi
  sudo ln -T -f -s "$install_dir" /opt/intellij/current
}

setup_xmonad() {
  log 'setting up xmonad'
  mkdir -p ~/.xmonad/
  ln -f -s "$script_dir/xmonad/xmonad.hs" ~/.xmonad/
  xmonad --recompile
  # TODO: start xmonad as WM
}

setup_bash() {
  log 'setting up bash'
  ln -f -s "$script_dir/bashrc" ~/.bashrc
  ln -f -s "$script_dir/bash_aliases" ~/.bash_aliases
  local bash_lib_path=~/.local/lib/bash
  if [[ ! -e $bash_lib_path ]]; then
    mkdir -p "$(dirname "$bash_lib_path")"
    git clone git@github.com:aduong/bash-libs.git "$bash_lib_path"
  fi
  if [[ ! -e ~/.bash_completion ]]; then
    local completion_dir=~/.bash_completion.d
    mkdir -p "$completion_dir"
    cat >~/.bash_completion <<EOF
#!/usr/bin/env bash

shopt nullglob > /dev/null
nullglob_enabled=$?

shopt -s nullglob
for f in $completion_dir/*; do
  . "\$f"
done

[[ \$nullglob_enabled -ne 0 ]] && shopt -u nullglob
EOF
  fi
}

setup_dircolors() {
  cp "$script_dir/dircolors" ~/.dircolors
}

setup_emacs() {
  log 'setting up emacs'
  ln -f -s "$script_dir/emacs" ~/.emacs
  # TODO: install emacs packages
  local desktop_file=~/.config/autostart/emacs\ server.desktop
  [[ -e $desktop_file ]] && return
  cat >"$desktop_file" <<EOF
[Desktop Entry]
Encoding=UTF-8
Version=0.9.4
Type=Application
Name=emacs server
Comment=
Exec=emacs --daemon
OnlyShowIn=XFCE;
RunHook=0
StartupNotify=false
Terminal=false
Hidden=false
EOF
}

setup_ssh() {
  local key_type=ed25519
  local privkey_path=~/.ssh/id_${key_type}
  [[ -e $privkey_path ]] && return
  log 'setting up ssh'
  ssh-keygen -t $key_type -f "$privkey_path" -N ''
  echo 'Pausing to give you time to add this key to GitHub, for example.'
  echo '[Enter] to continue'
  read -r
}

setup_git() {
  log 'setting up git'
  python3 -m pip install --user git-revise
  ln -f -s "$script_dir/gitconfig" ~/.gitconfig
}

setup_redshift() {
  local desktop_file=~/.config/autostart/redshift.desktop
  [[ -e $desktop_file ]] && return
  log 'setting up redshift'
  cat >"$desktop_file" <<EOF
[Desktop Entry]
Encoding=UTF-8
Version=0.9.4
Type=Application
Name=redshift
Comment=
Exec=/usr/bin/redshift
OnlyShowIn=XFCE;
RunHook=0
StartupNotify=false
Terminal=false
Hidden=false
EOF
}

setup_xfce() {
  log 'setting up xfce settings'
  cp "$script_dir/xfce4-terminal"/* ~/.config/xfce4/terminal/
  ln -s -f ~/.config/xfce4/terminal/{solarized-light,terminalrc}

  # TODO
  # cp "$script_dir/xfce4-config"/* ~/.config/xfce4/xfconf/xfce-perchannel-xml/
}

setup_gpg() {
  log 'setting up gpg'
  cp "$script_dir/gpg.conf" ~/.gnupg/
  # TODO?
}

setup_inotify() {
  log 'setting up inotify limits'
  local conf_file=/etc/sysctl.d/local.conf
  if grep -F fs.inotify.max_user_watches "$conf_file"; then
    sudo perl -i -pe 's/(fs.inotify.max_user_watches)\s*=\s*\d+/$1 = 524288/' "$conf_file"
  else
    sudo tee -a /etc/sysctl.d/local.conf >/dev/null <<<'fs.inotify.max_user_watches = 524288'
  fi
  sudo sysctl -p --system
}

install_sbt() {
  sudo tee /etc/apt/sources.list.d/sbt.list <<<'deb https://dl.bintray.com/sbt/debian /'
  curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
  sudo apt-get update
  sudo apt-get install -y sbt
}

setup_dns() {
  log 'setting up DNS (systemd-resolvd, dnsmasq, stubby)'
  sudo apt-get install -y dnsmasq stubby

  log 'stopping systemd, dnsmasq, stubby'
  sudo systemctl stop systemd-resolved.service
  sudo systemctl stop dnsmasq.service
  sudo systemctl stop stubby.service

  local stubby_port=5353

  # configure stubby
  sudo cp /etc/stubby/stubby.yml{,.bak}
  sudo tee /etc/stubby/stubby.yml >/dev/null <<EOF
resolution_type: GETDNS_RESOLUTION_STUB
dns_transport_list:
  - GETDNS_TRANSPORT_TLS
tls_authentication: GETDNS_AUTHENTICATION_REQUIRED
tls_query_padding_blocksize: 128
edns_client_subnet_private : 1
round_robin_upstreams: 1
idle_timeout: 10000
listen_addresses:
  - 127.0.0.1@$stubby_port
  - 0::1@$stubby_port
upstream_recursive_servers:
- address_data: 1.1.1.1
  tls_auth_name: "cloudflare-dns.com"
- address_data: 1.0.0.1
  tls_auth_name: "cloudflare-dns.com"
- address_data: 2606:4700:4700::1111
  tls_auth_name: "cloudflare-dns.com"
- address_data: 2606:4700:4700::1001
  tls_auth_name: "cloudflare-dns.com"
EOF

  # ensure stubby starts with -l for more visibility
  sudo perl -i -pe 's{^(ExecStart=/usr/bin/stubby).*}{$1 -l}' \
    /lib/systemd/system/stubby.service

  sudo systemctl daemon-reload
  log 'starting stubby'
  sudo systemctl start stubby.service

  # configure dnsmasq
  sudo tee /etc/dnsmasq.d/systemd-resolved >/dev/null <<EOF
bind-interfaces
EOF
  sudo tee /etc/dnsmasq.d/stubby >/dev/null <<EOF
no-resolv
proxy-dnssec
server=127.0.0.1#$stubby_port
server=::1#$stubby_port
listen-address=127.0.0.1
listen-address=::1
cache-size=1000
EOF

  log 'starting dnsmasq'
  sudo systemctl start dnsmasq.service

  # configure systemd-resolved
  sudo cp /etc/systemd/resolved.conf{,.bak}
  sudo tee /etc/systemd/resolved.conf >/dev/null <<EOF
[Resolve]
DNS=127.0.0.1
Domains=~.
EOF

  # ensure systemd-resolved runs after dnsmasq
  sudo mkdir -p /etc/systemd/system/dnsmasq.service.d
  sudo tee /etc/systemd/system/dnsmasq.service.d/resolved-fix.conf >/dev/null <<EOF
[Unit]
After=dnsmasq.service

[Service]
ExecStartPre=/usr/bin/systemctl stop systemd-resolved.service
ExecStartPost=/usr/bin/systemctl start systemd-resolved.service
EOF

  sudo systemctl daemon-reload
  log 'starting systemd-resolved'
  sudo systemctl start systemd-resolved.service
}


install_nerd_fonts() {
  local dejavu_path=/tmp/DejaVuSansMono.zip
  curl -o "$dejavu_path" -C - -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/DejaVuSansMono.zip
  sha512sum -c <<<"50b03224f43a319fe9db8a0845f5114811d8278b65d2ae1018f3529cad8ede1315438389ab132124b499ddbf779e0b4b195cef7e17dd0a3dc9c40657eed0ce1b  $dejavu_path"
  sudo unzip -d /usr/local/share/fonts "$dejavu_path"
  sudo fc-cache -v
}

nix_install() {
  if ! command -v nix-env; then
    sh <(curl -L https://nixos.org/nix/install) --daemon
  fi

  nix-channel --add https://nixos.org/channels/nixpkgs-unstable

  nix-env -i \
    autojump \
    bash_5 \
    bitwarden \
    chromium \
    curl \
    docker \
    emacs \
    firefox \
    git \
    gnupg \
    gopls \
    htop \
    iotop \
    jq \
    ntp \
    restic \
    ripgrep \
    mosh \
    nerdfonts \
    nodejs-15_x \
    openssh \
    powertop \
    redshift \
    spotify \
    starship \
    tlp \
    xclip \
    haskellPackages.xmonad \
    && :
}

main() {
  nix_install
  sudo apt-get update
  sudo apt-get upgrade -y

  setup_dns

  setup_ssh
  setup_bash
  setup_xmonad
  setup_emacs

  install_keybase
  install_intellij

  setup_git
  setup_redshift
  setup_xfce
  setup_inotify
}

if [[ $0 != bash ]]; then
  main
fi

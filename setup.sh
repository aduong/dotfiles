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

install_bitwarden() {
  sudo mkdir -p /opt/bitwarden
  local app_path=/opt/bitwarden/Bitwarden.AppImage
  if [[ ! -f $app_path ]]; then
    log 'installing bitwarden'
    sudo curl -L -o "$app_path" 'https://vault.bitwarden.com/download/?app=desktop&platform=linux'
    sudo chmod a+x "$app_path"
  fi
  mkdir -p ~/.local/share/applications
  cat > ~/.local/share/applications/bitwarden.desktop << EOF
[Desktop Entry]
Name=Bitwarden
Comment=A secure and free password manager for all of your devices.
Exec="/opt/bitwarden/Bitwarden.AppImage" %U
Terminal=false
Type=Application
Icon=appimagekit-bitwarden
StartupWMClass=Bitwarden
Categories=Utility;
TryExec=/opt/bitwarden/Bitwarden.AppImage
EOF
}

install_go() {
  local url go_version go_dir install_dir archive_path

  url=$(curl --silent https://golang.org/dl/ | grep -o -E 'https://[A-Za-z0-9/.]+\.linux-amd64\.tar\.gz' | head -1)
  go_version=$(basename "$url" .linux-amd64.tar.gz)
  go_dir=/opt/go
  install_dir=$go_dir/$go_version

  log 'installing go'
  if [[ ! -e $install_dir ]]; then
    archive_path="$(mktemp -d)/$go_version.tar.gz"
    curl -o "$archive_path" "$url"
    sudo mkdir -p "$install_dir"
    sudo tar --strip-components=1 -C "$install_dir" -xf "$archive_path"
  fi
  sudo ln -T -f -s "$install_dir" "$go_dir/current"
  sudo ln -f -s "$install_dir/bin/"* /usr/local/bin/

  go get -u golang.org/x/tools/cmd/goimports
}

install_node() {
  local url node_version node_dir install_dir
  url=$(curl --silent https://nodejs.org/en/download/ | grep -E -o 'https://nodejs.org/dist/v([0-9.]+)/node-v\1-linux-x64.tar.xz')
  node_version=$(basename "$url" -linux-x64.tar.xz)
  node_dir=/opt/node
  install_dir=$node_dir/$node_version

  if [[ ! -e $install_dir ]]; then
    log 'installing node'
    archive_path="$(mktemp -d)/$node_version.tar.xz"
    curl -o "$archive_path" "$url"
    sudo mkdir -p "$install_dir"
    sudo tar --strip-components=1 -C "$install_dir" -xf "$archive_path"
  fi
  sudo ln -T -f -s "$install_dir" "$node_dir/current"
  sudo ln -f -s "$install_dir/bin/"* /usr/local/bin/
}

install_intellij() {
  local version install_dir archive_path
  version=$(curl --silent 'https://data.services.jetbrains.com/products/releases?code=IIU&type=release&latest=true' | jq -r '.IIU[0].version')
  install_dir=/opt/intellij/$version
  if [[ ! -e $install_dir ]]; then
    log 'installing intellij'
    archive_path=$(mktemp -d)/ideaIU-$version
    curl -L -o "$archive_path" "https://download.jetbrains.com/idea/ideaIU-$version.tar.gz"
    sudo mkdir -p "$install_dir"
    sudo tar --strip-components=1 -C "$install_dir" -xf "$archive_path"
  fi
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
    cat > ~/.bash_completion <<EOF
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
  cat > "$desktop_file" <<EOF
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
  cat > "$desktop_file" <<EOF
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
  sudo tee -a /etc/sysctl.d/local.conf > /dev/null <<< 'fs.inotify.max_user_watches = 524288'
  sudo sysctl -p --system
}

install_shfmt() {
  log 'installing shfmt'
  GO111MODULE=on go get mvdan.cc/sh/v3/cmd/shfmt
}

install_sbt() {
  sudo tee -a /etc/apt/sources.list.d/sbt.list <<< 'deb https://dl.bintray.com/sbt/debian /'
  curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
  sudo apt-get update
  sudo apt-get install -y sbt
}

main() {
  sudo apt-get update
  sudo apt-get upgrade -y
  sudo apt-get install -y \
    autojump \
    chromium-browser \
    curl \
    emacs \
    firefox \
    git \
    gnupg2 \
    htop \
    jq \
    ntp \
    openjdk-8-jre \
    parallel \
    powertop \
    python3-pip \
    redshift \
    ripgrep \
    shellcheck \
    tlp \
    xmonad \
    xsel \
    && :

  sudo snap install \
    spotify \
    && :

  setup_ssh
  setup_bash
  setup_xmonad
  setup_emacs

  install_keybase
  install_bitwarden
  install_go
  install_node
  install_intellij

  setup_git
  setup_redshift
  setup_xfce
  setup_inotify

  install_shfmt
  install_sbt

  # TODO: docker
  sudo apt-get install -y docker.io
  sudo groupadd docker
}

if [[ $0 != bash ]]; then
  main
fi

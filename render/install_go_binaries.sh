#!/usr/bin/env bash

packages=(
  github.com/GoogleCloudPlatform/cloudsql-proxy/cmd/cloud_sql_proxy@latest
  github.com/aduong/gopkggraph@latest
  github.com/cespare/reflex@latest
  github.com/go-delve/delve/cmd/dlv@latest
  golang.org/x/tools/gopls@latest

)

cd
for pkg in "${packages[@]}"; do
  go install "$pkg"
done

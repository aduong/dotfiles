#!/usr/bin/env bash

packages=(
  github.com/GoogleCloudPlatform/cloudsql-proxy/cmd/cloud_sql_proxy@latest
  github.com/aduong/gopkggraph@latest
  github.com/cespare/reflex@latest
  github.com/golangci/golangci-lint/cmd/golangci-lint@v1.44.2
  golang.org/x/tools/gopls@latest
  # github.com/kubernetes-sigs/kustomize # seems to be broken as of 2021-04-20
)

cd
for pkg in "${packages[@]}"; do
  go install "$pkg"
done

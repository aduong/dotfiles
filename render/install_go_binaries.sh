#!/usr/bin/env bash

packages=(
  github.com/GoogleCloudPlatform/cloudsql-proxy/cmd/cloud_sql_proxy
  github.com/aduong/gopkggraph
  github.com/cespare/reflex
  github.com/golangci/golangci-lint/cmd/golangci-lint@v1.39.0
  golang.org/x/tools/gopls@latest
  # github.com/kubernetes-sigs/kustomize # seems to be broken as of 2021-04-20
)

cd
for pkg in "${packages[@]}"; do
  go install "$pkg"
done

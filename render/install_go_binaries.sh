#!/usr/bin/env bash

packages=(
  github.com/GoogleCloudPlatform/cloudsql-proxy/cmd/cloud_sql_proxy
  github.com/aduong/gopkggraph
  github.com/cespare/reflex
  github.com/golangci/golangci-lint/cmd/golangci-lint@v1.31.0
  github.com/kubernetes-sigs/kustomize
)

cd
for pkg in "${packages[@]}"; do
  GO111MODULE=on go get "$pkg"
done

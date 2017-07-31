#!/bin/bash

if [ ! -d "$HOME/tools/terraform-0.8.1" ]; then
  mkdir -p $HOME/tools/terraform-0.8.1
  cd $HOME/tools || exit
  curl -sLo terraform.zip https://releases.hashicorp.com/terraform/0.8.1/terraform_0.8.1_linux_amd64.zip
  unzip terraform.zip
  rm -f terraform.zip || true
  mv terraform terraform-0.8.1
fi

echo "terraform --version"
$HOME/tools/terraform-0.8.1/terraform --version

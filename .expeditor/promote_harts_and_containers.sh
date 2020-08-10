#!/bin/bash

set -eou pipefail

if [ "${EXPEDITOR_CHANNEL}" = "unstable" ];
then
  echo "This file does not need to be run for the unstable channel"
  exit 1
fi

# Export the HAB_AUTH_TOKEN for use of promoting habitat packages to {{TARGET_CHANNE}}
HAB_AUTH_TOKEN=$(vault kv get -field auth_token account/static/habitat/chef-ci)
export HAB_AUTH_TOKEN

# EXPEDITOR_VERSION and EXPEDITOR_CHANNEL are passed in via Expeditor when an omnibus package of
# version EXPEDITOR_VERSION is promoted to EXPEDITOR_CHANNEL

# Download the manifest
aws s3 cp "s3://chef-automate-artifacts/manifests/chef-server/${EXPEDITOR_VERSION}.json" manifest.json --profile chef-cd

# Download or create the versions file
aws s3 cp "s3://chef-automate-artifacts/${EXPEDITOR_CHANNEL}/latest/chef-server/versions.json" existing-versions.json --profile chef-cd || echo "[]" > existing-versions.json

# Promote the artifacts in Habitat Depot
jq -r -c ".packages[]" manifest.json | while read service_ident; do
  # service_ident will look like: chef/oc_erchef/12.18.2/20180806132701
  pkg_origin=$(echo $service_ident | cut -d / -f 1) # chef
  pkg_name=$(echo $service_ident | cut -d / -f 2) # oc_erchef
  pkg_version=$(echo $service_ident | cut -d / -f 3) # 12.18.2
  pkg_release=$(echo $service_ident | cut -d / -f 4) # 20180806132701

  if [ "$pkg_origin" = "core" ];
  then
    echo "Skipping promotion of core origin package ${service_ident}"
  else
    echo "Promoting ${service_ident} hart to the ${EXPEDITOR_CHANNEL} channel"
    hab pkg promote "${service_ident}" "${EXPEDITOR_CHANNEL}"
    # The pipeline has been improved, breaking this. I'm told to fix
    # it requires doing something different.
    #
    # TODO: remove this if we begin creating a container for `chef/openresty-noroot`
    # if [ "$pkg_name" = "openresty-noroot" ];
    # then
    #   echo "Skipping promotion of container for ${service_ident}"
    #   continue
    # fi
    #
    # echo "Promoting ${pkg_origin}/${pkg_name}:${pkg_version}-${pkg_release} container to ${EXPEDITOR_CHANNEL} tag"
    # docker pull "${pkg_origin}/${pkg_name}:${pkg_version}-${pkg_release}"
    # docker tag "${pkg_origin}/${pkg_name}:${pkg_version}-${pkg_release}" "${pkg_origin}/${pkg_name}:${EXPEDITOR_CHANNEL}"
    # docker push "${pkg_origin}/${pkg_name}:${EXPEDITOR_CHANNEL}"
    #
    # if [ "${EXPEDITOR_CHANNEL}" = "stable" ];
    # then
    #   docker tag "${pkg_origin}/${pkg_name}:${pkg_version}-${pkg_release}" "${pkg_origin}/${pkg_name}:latest"
    #   docker push "${pkg_origin}/${pkg_name}:latest"
    #   docker rmi "${pkg_origin}/${pkg_name}:latest"
    # fi
    #
    # docker rmi "${pkg_origin}/${pkg_name}:${pkg_version}-${pkg_release}" "${pkg_origin}/${pkg_name}:${EXPEDITOR_CHANNEL}"
  fi
done

# Append the new version to the target channel versions file
jq ". |= .+ [\"$(jq -r -c ".build" manifest.json)\"]" existing-versions.json > updated-versions.json

# Promote the License Scout Dependency Manifest
aws s3 cp "s3://chef-automate-artifacts/licenses/chef-server/${EXPEDITOR_VERSION}.json" "s3://chef-automate-artifacts/${EXPEDITOR_CHANNEL}/latest/chef-server/licenses.json" --acl public-read  --profile chef-cd
# Promote the manifest
aws s3 cp manifest.json "s3://chef-automate-artifacts/${EXPEDITOR_CHANNEL}/latest/chef-server/manifest.json" --acl public-read  --profile chef-cd
# Upload the updated versions file
aws s3 cp updated-versions.json "s3://chef-automate-artifacts/${EXPEDITOR_CHANNEL}/latest/chef-server/versions.json" --acl public-read  --profile chef-cd

# Cleanup
rm manifest.json
rm existing-versions.json
rm updated-versions.json

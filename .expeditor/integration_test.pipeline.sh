#!/usr/bin/env bash

prog='integration_test.pipeline.sh'
cloud=
action=

# number of seconds before AWS token expires
AWS_TOKEN_TIMEOUT=7200
export AWS_TOKEN_TIMEOUT

# point to consul backend
export CONSUL_HTTP_ADDR='http://consul.chef.co'

# verify command dependencies
[[ "$(command -v terraform)" ]] || error 'terraform command is not available'

# allow for environment override of build number
[[ -z "$BUILD_NUMBER" ]] && BUILD_NUMBER="${EXPEDITOR_BUILD_NUMBER}"
[[ -z "$BUILD_NUMBER" ]] && BUILD_NUMBER="${BUILDKITE_BUILD_NUMBER}"
export BUILD_NUMBER

# apply defaults if necessary
[[ -z "$TF_VAR_build_prefix" ]] && TF_VAR_build_prefix="${BUILD_NUMBER}-"
export TF_VAR_build_prefix
[[ -z "$TF_VAR_aws_contact" ]] && TF_VAR_aws_contact="${AWS_CONTACT:-chef-eng-team}"
export TF_VAR_aws_contact
[[ -z "$TF_VAR_aws_department" ]] && TF_VAR_aws_department="${AWS_DEPARTMENT:-CoreEng}"
export TF_VAR_aws_department
[[ -z "$TF_VAR_aws_profile" ]] && TF_VAR_aws_profile="${AWS_DEFAULT_PROFILE:-chef-cd}"
export TF_VAR_aws_profile
[[ -z "$TF_VAR_aws_region" ]] && TF_VAR_aws_region="${AWS_DEFAULT_REGION:-us-west-2}"
export TF_VAR_aws_region
[[ -z "$TF_VAR_aws_ssh_key_id" ]] && TF_VAR_aws_ssh_key_id="${AWS_SSH_KEY_ID:-cd-infrastructure}"
export TF_VAR_aws_ssh_key_id
[[ -z "$TF_VAR_aws_vpc_name" ]] && TF_VAR_aws_vpc_name="${AWS_VPC_NAME:-chef-eng-team-chef_server-test}"
export TF_VAR_aws_vpc_name
[[ -z "$TF_VAR_arm_contact" ]] && TF_VAR_arm_contact="${ARM_CONTACT:-chef-eng-team}"
export TF_VAR_arm_contact
[[ -z "$TF_VAR_arm_department" ]] && TF_VAR_arm_department="${ARM_DEPARTMENT:-CoreEng}"
export TF_VAR_arm_department
[[ -z "$TF_VAR_arm_tenant_id" ]] && TF_VAR_arm_tenant_id="${ARM_TENANT_ID:-a2b2d6bc-afe1-4696-9c37-f97a7ac416d7}"
export TF_VAR_arm_tenant_id
[[ -z "$TF_VAR_arm_subscription_id" ]] && TF_VAR_arm_subscription_id="${ARM_SUBSCRIPTION_ID:-80b824de-ec53-4116-9868-3deeab10b0cd}"
export TF_VAR_arm_subscription_id
[[ -z "$TF_VAR_arm_location" ]] && TF_VAR_arm_location="${ARM_LOCATION:-westus2}"
export TF_VAR_arm_location
[[ -z "$TF_VAR_arm_ssh_key_file" ]] && TF_VAR_arm_ssh_key_file="${ARM_SSH_KEY_FILE:-/workdir/id_rsa.pub}"
export TF_VAR_arm_ssh_key_file
[[ -z "$TF_VAR_arm_resource_group_name" ]] && TF_VAR_arm_resource_group_name="${ARM_RESOURCE_GROUP_NAME:-${BUILD_NUMBER}-chef-eng-team-chef_server-test}"
export TF_VAR_arm_resource_group_name

# print command usage
print_usage () {
    cat <<EOF
Usage: $prog [-h] [--help] <command>

Options:
    -h, --help  display this help and exit

Commands:

    apply       Applies the Terraform scenario configured via environment variables.
    destroy     Destroys the Terraform scenario based on environment variables.
    destroy-all Destroys all Terraform workspaces associated with the build number.

Environment Variables:

  REQUIRED:

    AWS_CONTACT
    AWS_DEFAULT_PROFILE
    AWS_DEFAULT_REGION
    AWS_DEPARTMENT
    AWS_SSH_KEY_ID
    AWS_VPC_NAME
    ENABLE_IPV6
    PLATFORM
    SCENARIO

  OPTIONAL:
    ACTION
    BUILD_NUMBER
EOF
}

# print error message followed by usage and exit
error () {
  local message="$1"

  echo -e "\nERROR: ${message}\n" >&2

  print_usage >&2

  exit 1
}

# process arguments
for arg in "$@"; do
  case "$arg" in
    -h | --help)
      print_usage
      exit 0
      ;;
    apply)
      action='apply'
      ;;
    destroy)
      action='destroy'
      ;;
    destroy-all)
      action='destroy-all'
      ;;
    aws)
      cloud='aws'
      ;;
    azure)
      cloud='azure'
      ;;
  esac
done

# setup the terraform workspace
setup () {
  local cloud=$1
  local scenario=$2
  local enable_ipv6=$3
  local platform=$4
  local workspace

  if [[ "$cloud" = 'azure' ]]; then
    echo "--- :azure: Setup Azure credentials"

    # this allows time for the new service-principal to become available
    sleep 60

    az login --service-principal --tenant "$ARM_TENANT_ID" --username "$ARM_CLIENT_ID" --password "$ARM_CLIENT_SECRET"

    if [[ -z "$(az group list --query "[?contains(name, '${TF_VAR_arm_resource_group_name}')].name" --output tsv)" ]]; then
      echo -e "--- :azure: Setup \033[38;5;62m\033[1m${TF_VAR_arm_resource_group_name}\033[0m resource group"

      cd "/workdir/terraform/azure/modules/arm_resource_group"
      if [[ ! -d .terraform ]]; then
      tfenv install min-required
      tfenv use min-required
      terraform init
      terraform apply -auto-approve
      fi
      cd "/workdir"
    fi
  fi

  if [[ "$enable_ipv6" = 'true' ]]; then
      workspace="${BUILD_NUMBER}-${scenario}-ipv6-${platform}"
  else
      workspace="${BUILD_NUMBER}-${scenario}-ipv4-${platform}"
  fi

  export TERRAFORM_WORKSPACE="$workspace"

  echo -e "--- :terraform: Initializing \033[38;5;62m\033[1m${workspace}\033[0m workspace"

  cd "/workdir/terraform/${cloud}/scenarios/${scenario}" || error "could not find ${scenario} scenario"

  # override terraform backend to use shared consul
  cat > "/workdir/terraform/${cloud}/scenarios/${scenario}/backend.tf" <<EOF
  terraform {
    backend "consul" {
      address = "http://consul.chef.co"
      scheme  = "https"
      path    = "terraform/chef-server/test-scenario"
      gzip    = true
    }
  }
EOF

  # initialize the terraform scenario
  if [[ ! -d .terraform ]]; then
	tfenv install min-required
	tfenv use min-required
	terraform init
  fi

  # switch terraform workspace
  terraform workspace select "$workspace" || terraform workspace new "$workspace"
}

# apply the terraform scenario
apply () {
  # verify command dependencies
  [[ "$(command -v mixlib-install)" ]] || error 'mixlib-install command is not available'
  [[ "$(command -v vault)" ]] || error 'vault command is not available'

  # apply defaults if necessary
  [[ -z "$TF_VAR_scenario" ]] && TF_VAR_scenario="${SCENARIO:-omnibus-standalone-fresh-install}"
  export TF_VAR_scenario
  [[ -z "$TF_VAR_enable_ipv6" ]] && TF_VAR_enable_ipv6="${ENABLE_IPV6:-true}"
  export TF_VAR_enable_ipv6
  [[ -z "$TF_VAR_platform" ]] && TF_VAR_platform="${PLATFORM:-ubuntu-18.04}"
  export TF_VAR_platform

  # apply optional test overrides if necessary
  [[ -n "$ENABLE_SMOKE_TEST" ]] && export TF_VAR_enable_smoke_test="$ENABLE_SMOKE_TEST"
  [[ -n "$ENABLE_PEDANT_TEST" ]] && export TF_VAR_enable_pedant_test="$ENABLE_PEDANT_TEST"
  [[ -n "$ENABLE_PSQL_TEST" ]] && export TF_VAR_enable_psql_test="$ENABLE_PSQL_TEST"
  [[ -n "$ENABLE_GATHER_LOGS_TEST" ]] && export TF_VAR_enable_gather_logs_test="$ENABLE_GATHER_LOGS_TEST"
  [[ -n "$ENABLE_ADDON_PUSH_JOBS" ]] && export TF_VAR_enable_addon_push_jobs="$ENABLE_ADDON_PUSH_JOBS"
  [[ -n "$ENABLE_ADDON_CHEF_MANAGE" ]] && export TF_VAR_enable_addon_chef_manage="$ENABLE_ADDON_CHEF_MANAGE"
  [[ -n "$ENABLE_CHEF_BACKEND_DEMOTION" ]] && export TF_VAR_enable_chef_backend_demotion="$ENABLE_CHEF_BACKEND_DEMOTION"

  # Allow Elasticsearch version to be overriden
  [[ -n "$ELASTIC_VERSION" ]] && export TF_VAR_elastic_version="$ELASTIC_VERSION"

  # setup the terraform workspace
  setup "$cloud" "$TF_VAR_scenario" "$TF_VAR_enable_ipv6" "${TF_VAR_platform}"

  case "$cloud" in
    aws)
      echo "--- :chef: Configure SSH key associated with $TF_VAR_aws_ssh_key_id from vault"
      eval "$(ssh-agent)"
      vault read -field=ssh_private_key "account/static/aws/${TF_VAR_aws_profile}/${TF_VAR_aws_ssh_key_id}" | ssh-add -
      [[ $(ssh-add -l | wc -l) -gt 0 ]] || error 'ssh-agent does not have any keys loaded!'
      ;;
    azure)
      echo "--- :chef: Configure SSH key from vault"
      vault read -field=ssh_public_key "account/static/aws/chef-cd/cd-infrastructure" > /workdir/id_rsa.pub
      eval "$(ssh-agent)"
      vault read -field=ssh_private_key "account/static/aws/chef-cd/cd-infrastructure" | ssh-add -
      [[ $(ssh-add -l | wc -l) -gt 0 ]] || error 'ssh-agent does not have any keys loaded!'
      ;;
  esac

  echo '--- :chef: Identify product versions and download URLs'
  [[ -z "$INSTALL_VERSION" ]] && INSTALL_VERSION="$(mixlib-install list-versions chef-server stable | tail -n 1)"
  export INSTALL_VERSION
  [[ -z "$UPGRADE_VERSION" ]] && UPGRADE_VERSION="$(mixlib-install list-versions chef-server current | tail -n 1)"
  export UPGRADE_VERSION
  [[ -z "$BACKEND_VERSION" ]] && BACKEND_VERSION="$(mixlib-install list-versions chef-backend current | tail -n 1)"
  export BACKEND_VERSION

  echo '--- :chef: Identify product download URLs'
  TF_VAR_install_version_url=$(for channel in unstable current stable; do mixlib-install download chef-server --url -c $channel -a x86_64 -p "$(sed 's/rhel/el/' <<<"${TF_VAR_platform%-*}")" -l "${TF_VAR_platform##*-}" -v "$INSTALL_VERSION" 2>/dev/null && break; done | head -n 1)
  export TF_VAR_install_version_url
  TF_VAR_upgrade_version_url=$(for channel in unstable current stable; do mixlib-install download chef-server --url -c $channel -a x86_64 -p "$(sed 's/rhel/el/' <<<"${TF_VAR_platform%-*}")" -l "${TF_VAR_platform##*-}" -v "$UPGRADE_VERSION" 2>/dev/null && break; done | head -n 1)
  export TF_VAR_upgrade_version_url
  TF_VAR_backend_version_url=$(for channel in unstable current stable; do mixlib-install download chef-backend --url -c $channel -a x86_64 -p "$(sed 's/rhel/el/' <<<"${TF_VAR_platform%-*}")" -l "${TF_VAR_platform##*-}" -v "$BACKEND_VERSION" 2>/dev/null && break; done | head -n 1)
  export TF_VAR_backend_version_url

  echo -e "+++ :terraform: Execute \033[38;5;62m\033[1m${TF_VAR_scenario}\033[0m scenario"

  #capture output to /workdir/integration_test.log
  {
    cat <<EOF

  BEGIN SCENARIO

      Workspace: $TERRAFORM_WORKSPACE
      Scenario: $TF_VAR_scenario
      Platform: $TF_VAR_platform
          IPv6: $TF_VAR_enable_ipv6
        Install: $INSTALL_VERSION
    Install URL: $TF_VAR_install_version_url
        Upgrade: $UPGRADE_VERSION
    Upgrade URL: $TF_VAR_upgrade_version_url
        Backend: $BACKEND_VERSION
    Backend URL: $TF_VAR_backend_version_url

EOF

    # run the terraform scenario
    terraform apply -auto-approve
    local ret=$?

    cat <<EOF

  END SCENARIO

      Workspace: $TERRAFORM_WORKSPACE
      Scenario: $TF_VAR_scenario
        Status: $( [[ "$ret" -eq 0 ]] && echo 'SUCCESS' || echo 'FAIL' )

EOF

    # capture paths from instances on failure for upload to buildkite
    if [[ $ret -ne 0 ]]; then
      echo -e "--- :chef: Gather content from instance for upload to Buildkite due to scenario failure"

      for f in /tmp/*connection_info.txt; do
        local target_dir="/workdir/$(sed 's_^/tmp/__;s_-connection.*$__;' <<<"$f")"

        mkdir -p "$target_dir"

        for capture_path in $(sed -n '2{s_[]\[,]_ _g;p;}' "$f"); do
          echo "Capturing $capture_path"

          tar -xzf <(ssh -o 'UserKnownHostsFile=/dev/null' -o 'StrictHostKeyChecking=no' "$(sed -n 1p "$f")" "sudo tar -czf - $capture_path") -C "$target_dir"
        done

        echo "Compressing captured content in $target_dir for upload to Buildkite."

        tar czf "${target_dir}.capture_paths.tar.gz" --transform 's/^workdir\///' "$target_dir"
        rm -rf "${target_dir}"
      done
    fi

    # destroy terraform scenario if aws token has not expired
    if [[ $SECONDS -lt $AWS_TOKEN_TIMEOUT ]]; then
      # allow destroy to fail
      destroy "$cloud" "$TERRAFORM_WORKSPACE" || true
    fi

    exit $ret
  } | tee /workdir/integration_test.log

  exit ${PIPESTATUS[0]}
}

# destroy the terraform scenario
destroy () {
  local cloud="$1"
  local workspace="$2"
  local ret=0

  if [[ -n "$workspace" ]]; then
    # extract values from workspace name
    TF_VAR_scenario=$(sed 's/^[0-9]*-//;s/-ipv.-.*//' <<<"$workspace")
    TF_VAR_enable_ipv6=$(grep -q '\-ipv6-' <<<"$workspace" && echo true || echo false)
    TF_VAR_platform="${workspace/*ipv?-/}"
  else
    # read values from environment if workspace was not directly passed in
    [[ -z "$TF_VAR_scenario" ]] && TF_VAR_scenario="${SCENARIO:-omnibus-standalone-fresh-install}"
    [[ -z "$TF_VAR_enable_ipv6" ]] && TF_VAR_enable_ipv6="${ENABLE_IPV6:-true}"
    [[ -z "$TF_VAR_platform" ]] && TF_VAR_platform="${PLATFORM:-ubuntu-18.04}"

    if [[ "$TF_VAR_enable_ipv6" = 'true' ]]; then
        workspace="${BUILD_NUMBER}-${TF_VAR_scenario}-ipv6-${TF_VAR_platform}"
    else
        workspace="${BUILD_NUMBER}-${TF_VAR_scenario}-ipv4-${TF_VAR_platform}"
    fi
  fi

  export TF_VAR_scenario
  export TF_VAR_enable_ipv6
  export TF_VAR_platform

  # ensure the workspace is available
  setup "$cloud" "$TF_VAR_scenario" "$TF_VAR_enable_ipv6" "${TF_VAR_platform}"

  echo "^^^ +++"
  echo -e "--- :terraform: Destroying \033[38;5;62m\033[1m${workspace}\033[0m workspace"

  # set bogus values for destroy
  export TF_VAR_install_version_url='NULL'
  export TF_VAR_upgrade_version_url='NULL'
  export TF_VAR_backend_version_url='NULL'

  if terraform destroy -auto-approve; then
    terraform workspace select default || echo 'ERROR: terraform failed to switch to the "default" workspace!'
    if ! terraform workspace delete "$workspace"; then
      echo "ERROR: terraform failed to delete the $workspace workspace! Manual cleanup of resources may be required"
      ret=1
    fi
  else
    echo 'ERROR: terraform destroy failed! Manual cleanup of resources may be required'
    ret=1
  fi

  return $ret
}

# destroy all terraform scenarios
destroy-all () {
  local workspace
  local ret=0

  echo "--- Destroying all Terraform workspaces associated with build number $BUILD_NUMBER"

  # ensure consul is installed
  if [[ ! "$(command -v consul)" ]]; then
    asdf plugin-add consul
    asdf install consul 1.6.2
    asdf local consul 1.6.2
  fi

  # iterate across workspaces left in consul
  for workspace in $(consul kv get -keys terraform/chef-server/ | sed -n "/${BUILD_NUMBER}/{s/.*:\(${BUILD_NUMBER}-.*$\)/\1/;s/\///;p;}" | sort -u); do
    local cloud="$(consul kv get terraform/chef-server/test-scenario-env:${workspace} | gunzip 2>/dev/null | grep -q aws && echo 'aws' || echo 'azure')"
    destroy "$cloud" "$workspace" || ret=1
  done

  exit $ret
}

# allow expeditor to override ACTION when a cancellation or failure occurred
[[ "$EXPEDITOR_BUILD_STATE" =~ ^(canceled|failed)$ ]] && ACTION='destroy-all'

# allow for environment override of action
case "$ACTION" in
  apply)
    action='apply'
    ;;
  destroy)
    action='destroy'
    ;;
  destroy-all)
    action='destroy-all'
    ;;
esac

# verify we have an action set
[[ -z "$action" ]] && error 'no action provided'

# execute desired action
eval "$action"

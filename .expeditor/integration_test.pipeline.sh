#!/usr/bin/env bash

prog='integration_test.pipeline.sh'
action=

# number of seconds before AWS token expires
AWS_TOKEN_TIMEOUT=3600
export AWS_TOKEN_TIMEOUT

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
  esac
done

# setup the terraform workspace
setup () {
  local scenario=$1
  local enable_ipv6=$2
  local platform=$3
  local workspace

  if [[ "$enable_ipv6" = 'true' ]]; then
      workspace="${BUILD_NUMBER}-${scenario}-ipv6-${platform}"
  else
      workspace="${BUILD_NUMBER}-${scenario}-ipv4-${platform}"
  fi

  export TERRAFORM_WORKSPACE="$workspace"

  echo "--- Initializing $workspace workspace"

  cd "/workdir/terraform/aws/scenarios/${scenario}" || error "could not find ${scenario} scenario"

  # override terraform backend to use shared consul
  cat > "/workdir/terraform/aws/scenarios/${scenario}/backend.tf" <<EOF
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
  [[ -d .terraform ]] || terraform init

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

  # setup the terraform workspace
  setup "$TF_VAR_scenario" "$TF_VAR_enable_ipv6" "${TF_VAR_platform}"

  echo "--- Configure SSH key associated with $TF_VAR_aws_ssh_key_id from vault"
  eval "$(ssh-agent)"
  vault read -field=ssh_private_key "account/static/aws/${TF_VAR_aws_profile}/${TF_VAR_aws_ssh_key_id}" | ssh-add -
  [[ $(ssh-add -l | wc -l) -gt 0 ]] || error 'ssh-agent does not have any keys loaded!'

  echo '--- Identify product versions and download URLs'
  [[ -z "$INSTALL_VERSION" ]] && INSTALL_VERSION="$(mixlib-install list-versions chef-server stable | tail -n 1)"
  export INSTALL_VERSION
  [[ -z "$UPGRADE_VERSION" ]] && UPGRADE_VERSION="$(mixlib-install list-versions chef-server current | tail -n 1)"
  export UPGRADE_VERSION
  [[ -z "$BACKEND_VERSION" ]] && BACKEND_VERSION="$(mixlib-install list-versions chef-backend current | tail -n 1)"
  export BACKEND_VERSION

  echo '--- Identify product download URLs'
  TF_VAR_install_version_url=$(for channel in unstable current stable; do mixlib-install download chef-server --url -c $channel -a x86_64 -p "$(sed 's/rhel/el/' <<<"${TF_VAR_platform%-*}")" -l "${TF_VAR_platform##*-}" -v "$INSTALL_VERSION" 2>/dev/null && break; done | head -n 1)
  export TF_VAR_install_version_url
  TF_VAR_upgrade_version_url=$(for channel in unstable current stable; do mixlib-install download chef-server --url -c $channel -a x86_64 -p "$(sed 's/rhel/el/' <<<"${TF_VAR_platform%-*}")" -l "${TF_VAR_platform##*-}" -v "$UPGRADE_VERSION" 2>/dev/null && break; done | head -n 1)
  export TF_VAR_upgrade_version_url
  TF_VAR_backend_version_url=$(for channel in unstable current stable; do mixlib-install download chef-backend --url -c $channel -a x86_64 -p "$(sed 's/rhel/el/' <<<"${TF_VAR_platform%-*}")" -l "${TF_VAR_platform##*-}" -v "$BACKEND_VERSION" 2>/dev/null && break; done | head -n 1)
  export TF_VAR_backend_version_url

  echo "--- Execute $TF_VAR_scenario scenario"
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

  # destroy terraform scenario if aws token has not expired
  if [[ $SECONDS -lt $AWS_TOKEN_TIMEOUT ]]; then
    # allow destroy to fail
    destroy "$TERRAFORM_WORKSPACE" || true
  fi

  exit $ret
}

# destroy the terraform scenario
destroy () {
  local workspace="$1"
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
  setup "$TF_VAR_scenario" "$TF_VAR_enable_ipv6" "${TF_VAR_platform}"

  echo "--- Destroying $workspace"

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

  # verify command dependencies
  [[ "$(command -v consul)" ]] || error 'consul command is not available'

  # iterate across workspaces left in consul
  for workspace in $(consul kv get -keys terraform/chef-server/ | sed -n "/${BUILD_NUMBER}/{s/.*:\(${BUILD_NUMBER}-.*$\)/\1/;s/\///;p;}" | sort -u); do
    destroy "$workspace" || ret=1
  done

  exit $ret
}

echo '--- Verifying environment'

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

#!/usr/bin/env bash
#shellcheck disable=SC2034
#shellcheck disable=SC1117
#shellcheck disable=SC1090
#shellcheck disable=SC1091

set -eo pipefail
OLD_VERSION="latest"
test_name=""
test_upgrades=false
test_backup_restore=false
test_channel="dev"
test_upgrade_strategy="none"
# shellcheck disable=SC2034
test_build_slug="${test_build_slug:-"$RANDOM-$RANDOM"}"
test_container_name="${test_container_name:-"automate-${test_build_slug}"}"
test_loadbalancer_url="https://localhost"
test_notifications_endpoint="http://localhost:15555"
test_deploy_inspec_profiles=()
test_upgrade_inspec_profiles=()
test_skip_diagnostics=false
test_diagnostics_filters=""
test_diagnostics_pre_upgrade_filters=""
test_diagnostics_opts=""
test_external_services=()
test_proxy=false
test_proxy_container_name="automate-proxy-${test_build_slug}"
test_proxy_internal_network_name="automate-network-${test_build_slug}"

# test_detect_broken_cli detects if we broke the cli. There was a
# bug in older CLIs where it was possible to download a partial
# automate-cli binary. Once this happens, all uses of the cli
# fail silently. We can't go back in time and fix the problem.
# Tests that deploy an older habitat will likely need to set this
# to true to avoid random failures.
# See https://github.com/chef/automate/pull/3257
test_detect_broken_cli=false

# test_detect_broken_packages attempts to detect if we broke habitat
# packages. There were bugs in older A2 and Habitat versions that made
# this possible. Once this happens, deployment-service can't always
# recover. Tests that deploy an older habitat will likely need to set
# this to true to avoid failures.
#
# See https://github.com/chef/automate/pull/3050
# See https://github.com/habitat-sh/habitat/pull/5369
# See https://github.com/habitat-sh/habitat/issues/5317
test_detect_broken_packages=false

test_with_s3=false

# Manifest Management
#
# * Available Manifests
#
# build.json:        manifest created during the build process. Includes
#                    packages from unstable AND any newly created packages.
#
# build-habdev.json: Like build.json but with habitat packages from
#                    unstable as well.

# dev.json:          manifest for the dev channel
#
# acceptance.json:   manifest for the acceptance channel
#
# current.json:      manifest for the current channel
#
# test_manifest_path is the path that will be passed to `chef-automate
# deploy`. Manifest content is copied to this path before the deploy
# and before the upgrade. You should not normally need to change this
# variable.
declare -r test_manifest_path="local_manifest.json"
declare -r test_versions_path="local_versions.json"
#
# test_manifest_dir is the directory where we save all manifests
# to. You should not normally need to change this variable.
declare -r test_manifest_dir="/tmp/manifests"
#
# test_upgrade_begin_manifest is the filename of the manifest that
# will be used for the INITIAL deploy in the upgrade tests. That is,
# you will be upgrading FROM the release specified in this
# variable. We look for the filenames in test_manifest_dir.
test_upgrade_begin_manifest="dev.json"
declare -r test_config_path="config.toml"
declare -r test_hartifacts_path="results/"

# TODO(jaym): rename. tmp isn't descriptive
 # shellcheck disable=SC2034
declare -r test_tmp_hartifacts_path="tmp_results/"

set_test_manifest() {
    local target_manifest_name=$1
    cp "$test_manifest_dir/$target_manifest_name" "$test_manifest_path"
}

set_test_versions() {
    local target_versions_name=$1
    cp "$test_manifest_dir/$target_versions_name" "$test_versions_path"
}

do_setup() {
    do_setup_default
}

do_setup_default() {
    umask 077
    echo "umask 077" >> /etc/profile
    # this is needed for the preflight checks to pass
    sysctl -w vm.dirty_expire_centisecs=20000
    sysctl -w vm.max_map_count=262144

    if [[ "${test_proxy}" = "true" ]]; then
        log_info "Setting up proxy"
        HTTP_PROXY="http://${test_proxy_container_name}:3128"
        http_proxy="http://${test_proxy_container_name}:3128"
        HTTPS_PROXY="http://${test_proxy_container_name}:3128"
        https_proxy="http://${test_proxy_container_name}:3128"
        export HTTP_PROXY
        export http_proxy
        export HTTPS_PROXY
        export https_proxy
        npm config set proxy "http://${test_proxy_container_name}:3128"
        npm config set https-proxy "http://${test_proxy_container_name}:3128"
    fi

    start_requestbin

}

do_build() {
    do_build_default
}

do_build_default() {
    previous_umask=$(umask)
    umask 022
    if [[ "$BUILDKITE" = "true" ]]; then
       log_info "Downloading pre-build artifacts"
       download_hartifacts
    fi

    mkdir $test_manifest_dir

    if [[ -f "$test_hartifacts_path/.prebuilt_artifacts" ]]; then
        log_info "Using pre-built Hartifacts and Manifests"
        cp results/*.json $test_manifest_dir
    else
        log_info "No pre-built sentinel file found"
        log_info "Building Changes"
        #build_changed
        log_info "Downloading channel manifests"
        download_manifest "dev" "$test_manifest_dir/dev.json"
        download_manifest "acceptance" "$test_manifest_dir/acceptance.json"
        download_manifest "current" "$test_manifest_dir/current.json"
        log_info "Creating build.json"
        create_manifest "$test_manifest_dir/build.json"
    fi

    log_info "Building Tools"
    build_tools
    log_info "Copying packages to hab artifact cache"
    copy_hartifacts "$test_hartifacts_path"
    umask "$previous_umask"
}

do_create_config() {
    do_create_config_default
}

do_create_config_default() {
    local cli_bin="chef-automate"
    if [ $test_upgrades = true ]; then
      # Install the automate CLI from current
      cli_bin="/bin/chef-automate-latest"
      download_cli "${OLD_VERSION}" "${cli_bin}"
    fi
    ${cli_bin} init-config \
        --channel $test_channel \
        --file "$test_config_path" \
        --upgrade-strategy "$test_upgrade_strategy" \
        --es-mem "2g"
    cat >> "$test_config_path" <<EOF
[deployment.v1.sys.log]
  level = "debug"
[dex.v1.sys.expiry]
  id_tokens = "5m"
[postgresql.v1.sys.pg]
  shared_buffers = "1GB"
EOF
}

do_prepare_deploy() {
    do_prepare_deploy_default
}

do_prepare_deploy_default() {
    if [ $test_upgrades = true ]; then
        # temporarily move everything from results to the side so that
        # everything is deployed from dev
        mkdir -p "$test_tmp_hartifacts_path"
        mv -f $test_hartifacts_path/* "$test_tmp_hartifacts_path/" || true
        set_test_manifest "$test_upgrade_begin_manifest"
    else
        set_test_manifest "build.json"
    fi
}

do_deploy() {
    do_deploy_default
    do_apply_license
}

do_apply_license(){
    chef-automate license apply "$A2_LICENSE"
}

do_deploy_default() {
    chef-automate deploy "$test_config_path" \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa \
        --debug 
}

do_test_deploy() {
    do_test_deploy_default
}

do_test_deploy_default() {
    if [ $test_skip_diagnostics = false ]; then
        run_diagnostics_pre_upgrade $test_loadbalancer_url "$test_diagnostics_filters" "$test_diagnostics_pre_upgrade_filters" \
            "$test_diagnostics_opts"
    fi

    run_inspec_tests "$A2_ROOT_DIR" "${test_deploy_inspec_profiles[@]}"
    no_panic_check
}

do_prepare_upgrade() {
    set_version_file
    do_prepare_upgrade_default
    append_version_file
}

append_version_file() {
    #prepare the versions.json file
    #todo: vivek shankar build/version by schema_version
    newversion=$(jq -r -c ".build"  "$test_manifest_path")
    echo $newversion
    jq --arg val $newversion '. + [$val]' "$versionsFile" > tmp.$$.json && mv tmp.$$.json "$versionsFile"
}

set_version_file() {
    hab pkg install --binlink core/jq-static --force

    #prepare the versions.json file
    newversion=$(jq -r -c ".build"  "$test_manifest_path")
    echo $newversion
    versionsFile="/tmp/versions.json"
    echo '[]' > $versionsFile
    jq --arg val $newversion '. + [$val]' "$versionsFile" > tmp.$$.json && mv tmp.$$.json "$versionsFile"
}

prepare_upgrade_milestone(){
  local channel="$1"
  local version="$2"
  # shellcheck disable=SC2154
  download_manifest_version "$channel" "$version" "$test_manifest_dir/$version.json"
  set_test_manifest "$version.json"
  set_version_file
}

prepare_upgrade_milestone_append_version(){
  local channel="$1"
  local version="$2"
  # shellcheck disable=SC2154
  download_manifest_version "$channel" "$version" "$test_manifest_dir/$version.json"
  set_test_manifest "$version.json"
  append_version_file
}

do_prepare_upgrade_default() {
    # Move the newly built packages into the hartifacts directory.
    mv -f $test_tmp_hartifacts_path/* "$test_hartifacts_path/" || true
    set_test_manifest "build.json"
}

do_upgrade() {
    do_upgrade_default
}

do_upgrade_default() {
    run_upgrade
}

do_test_upgrade() {
    do_test_upgrade_default
}

do_test_upgrade_default() {
    if [ $test_skip_diagnostics = false ]; then
        run_diagnostics_post_upgrade $test_loadbalancer_url "$test_diagnostics_filters" "$test_diagnostics_pre_upgrade_filters" \
            "$test_diagnostics_opts"
    fi

    run_inspec_tests "$A2_ROOT_DIR" "${test_upgrade_inspec_profiles[@]}"
    no_panic_check
}

do_backup() {
    do_backup_default
}

do_backup_default() {
    chef-automate backup create

    # Get the backup id
    test_backup_id=$(chef-automate backup list | tail -1 | awk '{print $1}')
    backup_sha256=$(chef-automate backup show "$test_backup_id" | grep SHA256 | cut -d ' ' -f 4)
}

do_prepare_restore() {
    do_prepare_restore_default
}

do_prepare_restore_default() {
    systemctl stop chef-automate
    # Leave /hab/pkgs and /hab/cache
    # Delete these sylinks
    rm -rf /bin/knife
    rm -rf /bin/chef-server-ctl
    rm -rf /bin/chef-automate
    rm -rf "/hab/bin"
    rm -rf "/hab/sup"
    rm -rf "/hab/svc"
    rm -rf "/hab/user"
    rm -rf "/hab/launcher"
    copy_hartifacts "$test_hartifacts_path"
}

do_restore() {
    do_restore_default
}

do_restore_default() {
    ## ??? does pipefail make this fail inside a function or do we need -E for
    ## that or something else? :bashes:
    test_incorrect_restore_checksums_fail || return 1
    chef-automate backup restore --debug --override-origin "$HAB_ORIGIN" --sha256 "$backup_sha256" "$test_backup_id"
}

do_test_restore() {
    do_test_restore_default
}

do_test_restore_default() {
    # shellcheck disable=SC2086
    chef-automate diagnostics run $test_diagnostics_filters ~remove-this-tag-after-merge --skip-generate
}

do_cleanup() {
    :
}


do_dump_logs() {
    do_dump_logs_default
}

do_dump_logs_default() {
    :
}


# if we encounter an error we should dump the logs
dump_logs() {
    errcode=$?
    break_log "remaining"
    journalctl -u chef-automate -u requestbin -u elasticsearch --no-pager > logs/all
    do_dump_logs

    if command -v buildkite-agent
    then
        if [ "$errcode" -ne 0 ]; then
            echo "Deploy Failed - Uploading logs to buildkite"
        fi

        if [[ -d /usr/share/elasticsearch/logs ]]; then
            if ! buildkite-agent artifact upload "/usr/share/elasticsearch/logs/*"; then
                echo "Failed to upload elasticsearch logs"
            fi
        fi


        if ! buildkite-agent artifact upload "logs/*"
        then
            echo "Failed to upload logs... Dumping"
            cat logs/all
        fi
    fi

    log_info "Killing background processes"
    for j in $(jobs -p); do
        echo "killing $j"
        kill -9 "$j"
    done

    log_info "Exiting with $errcode"
    exit $errcode
}

last_cursor=""
break_log() {
    local name="$1"
    local cursor="$last_cursor"
    last_cursor=$(journalctl -u chef-automate -u requestbin -u elasticsearch --show-cursor -n 0 | awk '/cursor/{ print $3 }')
    mkdir -p logs
    if [ -z "$cursor" ]; then
        journalctl -u chef-automate -u requestbin -u elasticsearch --no-pager > "logs/$name"
    else
        journalctl -u chef-automate -u requestbin -u elasticsearch -c "$cursor" --no-pager > "logs/$name"
    fi
}

inherit() {
    local basepath="$(dirname ${BASH_SOURCE[1]})"
    source "${basepath}/$1"
}

source_dir=$(cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd)
export A2_ROOT_DIR="$source_dir/../"

# Load the shared code
# shellcheck source=./helpers/log.sh
source "${source_dir}/helpers/log.sh"
# shellcheck source=./helpers/setup.sh
source "${source_dir}/helpers/setup.sh"
# shellcheck source=./helpers/build.sh
source "${source_dir}/helpers/build.sh"
# shellcheck source=./helpers/deployment.sh
source "${source_dir}/helpers/deployment.sh"
# shellcheck source=./helpers/testing.sh
source "${source_dir}/helpers/testing.sh"
# shellcheck source=./helpers/studio_stubs.sh
source "${source_dir}/helpers/studio_stubs.sh"
# shellcheck source=./helpers/backup.sh
source "${source_dir}/helpers/backup.sh"
# shellcheck source=./helpers/hab.sh
source "${source_dir}/helpers/hab.sh"

__run_test() {
    trap dump_logs EXIT
    # Load the test definition
    # shellcheck source=/dev/null
    log_info "Loading test definition"
    source "$1"

    log_info "Running test $test_name on $test_container_name"

    log_section_start "Step do_setup"
    do_setup

    log_section_start "Step do_build"
    do_build

    log_section_start "Step do_create_config"
    do_create_config

    log_section_start "Step do_prepare_deploy"
    do_prepare_deploy

    log_section_start "Step do_deploy"
    do_deploy
    break_log "deploy"

    log_section_start "Step do_test_deploy"
    do_test_deploy
    break_log "test_deploy"

    if [ $test_upgrades = true ]; then
        log_section_start "Step do_prepare_upgrade"
        do_prepare_upgrade
        break_log "prepare_upgrade"

        log_section_start "Step do_upgrade"
        do_upgrade
        break_log "upgrade"

        log_section_start "Step do_test_upgrade"
        do_test_upgrade
        break_log "test_upgrade"
    fi


    if [ $test_backup_restore = true ]; then
        log_section_start "Step do_backup"
        do_backup
        break_log "backup"

        log_section_start "Step do_prepare_restore"
        do_prepare_restore

        log_section_start "Step do_restore"
        do_restore
        break_log "restore"

        log_section_start "Step do_test_restore"
        do_test_restore
        break_log "test_restore"
    fi

    log_section_start "Step do_cleanup"
    do_cleanup
}

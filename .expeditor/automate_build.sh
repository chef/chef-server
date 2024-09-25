#!/bin/bash -e

# =======================================================building Chef-server components=======================================================
export CHEF_SERVER_SRC='/workdir/src'
export HAB_ORIGIN=cheftest
export HAB_LICENSE=accept-no-persist


curl https://raw.githubusercontent.com/habitat-sh/habitat/main/components/hab/install.sh | sudo bash

export JOB_TEMP_ROOT
JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export HAB_CACHE_KEY_PATH
HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

echo "--- :key: Generating fake origin key"
hab license accept
hab origin key generate cheftest
#bookshelf oc_bifrost oc_erchef oc-id
for PACKAGE_NAME in openresty-noroot nginx ; do

    if [[ "$PACKAGE_NAME" == "nginx" ]]; then
        plan_sh_change="src/$PACKAGE_NAME/habitat/plan.sh"
        pushd results
        openrestyFilehart=$(ls -1t *openresty*.hart | head -1)
        popd
        echo "openresty-noroot package hart file is this: $openrestyFilehart"
        base_name=$(basename "$openrestyFilehart")
        IFS='-' read -r some name comp version timestamp os <<< "${base_name%.hart}"
        formatted_output="openresty-noroot/$version/$timestamp"
        sed -i "s|\${HAB_ORIGIN:-chef}/openresty-noroot|cheftest/${formatted_output}|g" "$plan_sh_change"
        cat $plan_sh_change
        echo "generating package for $PACKAGE_NAME"
        HAB_FEAT_IGNORE_LOCAL=true HAB_ORIGIN=cheftest HAB_CACHE_KEY_PATH=$HAB_CACHE_KEY_PATH DO_CHECK=true hab studio run -D "hab pkg build src/$PACKAGE_NAME"
    else

echo "generating package for $PACKAGE_NAME"
HAB_FEAT_IGNORE_LOCAL=true HAB_ORIGIN=cheftest HAB_CACHE_KEY_PATH=$HAB_CACHE_KEY_PATH DO_CHECK=true hab studio run -D "hab pkg build src/$PACKAGE_NAME"
fi
done

# =======================================================building Chef-server components With Automate=======================================================

export OCTOKIT_ACCESS_TOKEN
export HAB_LICENSE=accept
export CHEF_LICENSE="accept-no-persist"
export CI=true
export HAB_ORIGIN=cheftest
export HAB_ORIGIN_KEYS=cheftest
export HAB_STUDIO_SECRET_HAB_FEAT_IGNORE_LOCAL=true
export HAB_FEAT_IGNORE_LOCAL=true
export HAB_STUDIO_HOST_ARCH=x86_64-linux

git clone https://github.com/chef/automate.git
cd automate

git checkout kalroy/cs_plan_changes

cp ../results/*.hart results/

../.expeditor/replace.sh

cp $HAB_CACHE_KEY_PATH/* results/

RESOLVED_RESULTS_DIR=$(realpath results/)
HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR
HAB_FEAT_OFFLINE_INSTALL=true
ls $HAB_CACHE_KEY_PATH

name_resolver() {
    local package_name="$1"
    echo $(find components -name  "automte-cs*$package_name*" -type d)
}

#bookshelf bifrost erchef id
for PACKAGE_NAME in  nginx ; do
hart_file=$(ls results/*$PACKAGE_NAME*.hart)
output_string_file=$(echo "$hart_file" | sed 's|results/||')
echo "hab pkg install $hart_file"
HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=true HAB_ORIGIN=cheftest HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "source .studiorc; set -e; hab pkg install results/$output_string_file; hab pkg build $(name_resolver $PACKAGE_NAME)"
# HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=true HAB_ORIGIN=chef HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "source .studiorc; set -e; hab pkg install results/$output_string_file; hab pkg build components/$(name_resolver $PACKAGE_NAME)"
done

tar -cvf results.tar results
echo "results.tar created"
gzip results.tar

# buildkite-agent artifact upload results.tar.gz
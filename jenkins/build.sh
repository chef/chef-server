#!/bin/bash -xe

# Assert that $GIT_TAG is an annotated tag, if it exists.
if [ -n "$GIT_TAG" ]; then git tag -v $GIT_TAG; fi

if [ ! -e /opt/opscode/.branch ]; then exit 1; fi
export GIT_BRANCH_CLEAN="$(echo "$GIT_BRANCH" | tr / _)"
if [ -z "$GIT_BRANCH_CLEAN" ]; then exit 1; fi
if [ "$GIT_BRANCH_CLEAN" != "$(cat /opt/opscode/.branch)" ]
then
  sudo mv /opt/opscode "/opt/opscode-$(cat /opt/opscode/.branch)"
  if [ ! -d "/opt/opscode-$GIT_BRANCH_CLEAN" ]; then sudo mkdir "/opt/opscode-$GIT_BRANCH_CLEAN"; echo "$GIT_BRANCH_CLEAN" | sudo tee "/opt/opscode-$GIT_BRANCH_CLEAN/.branch"; fi
  sudo mv "/opt/opscode-$GIT_BRANCH_CLEAN" /opt/opscode
fi
sudo env CHEF_SOLO_COOKBOOKS="$(pwd)/cookbooks" chef-solo -c /srv/opscode-omnibus/shared/solo.rb -j /srv/opscode-omnibus/shared/solo.json
if [ ! -d "/var/cache/omnibus/${GIT_BRANCH_CLEAN}" ]; then mkdir "/var/cache/omnibus/${GIT_BRANCH_CLEAN}"; fi
cp omnibus.rb.example omnibus.rb
cat <<OMNIBUS_CONFIG >>omnibus.rb
Omnibus.configure do |o|
  o.cache_dir = "/var/cache/omnibus/${GIT_BRANCH_CLEAN}/cache"
  o.source_dir = "/var/cache/omnibus/${GIT_BRANCH_CLEAN}/src"
  o.build_dir = "/var/cache/omnibus/${GIT_BRANCH_CLEAN}/build"
  o.package_dir = "/var/cache/omnibus/${GIT_BRANCH_CLEAN}/pkg"
end
OMNIBUS_CONFIG

rm -r /var/cache/omnibus/${GIT_BRANCH_CLEAN}/pkg/* || true
rm pkg/* || true

export PATH=/usr/local/bin:$PATH
bundle install --deployment --without development
bundle exec rake projects:private-chef
# Cleanup
if [ "${GIT_BRANCH}" != "master" ]
then
  sudo rm -rf "/var/cache/omnibus/${GIT_BRANCH_CLEAN}"
  sudo rm -rf /opt/opscode
  if [ -d /opt/opscode-master ]
  then
    sudo mv /opt/opscode-master /opt/opscode
  fi
fi

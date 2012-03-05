#!/bin/bash -xe
if [ ! -e /opt/opscode/.branch ]; then exit 1; fi
export GIT_BRANCH_CLEAN=""
for SHA in $(git rev-list HEAD)
do
  GIT_BRANCH_POSSIBLE="$(git show-ref | grep -v tags | grep -v HEAD | grep "$SHA")"
  if [ -n "$GIT_BRANCH_POSSIBLE" ]; then break; fi
done
if [ -z "$GIT_BRANCH_POSSIBLE" ]; then exit 1; fi
export GIT_BRANCH_CLEAN="$(echo "$GIT_BRANCH_POSSIBLE" | awk '{print $2}' | sed s/refs\\/remotes\\/origin\\/// | tr / _)"
if [ -z "$GIT_BRANCH_CLEAN" ]; then exit 1; fi
if [ "$GIT_BRANCH_CLEAN" != "$(cat /opt/opscode/.branch)" ]
then
  sudo mv /opt/opscode "/opt/opscode-$(cat /opt/opscode/.branch)"
  if [ ! -d "/opt/opscode-$GIT_BRANCH_CLEAN" ]; then sudo mkdir "/opt/opscode-$GIT_BRANCH_CLEAN"; sudo echo "$GIT_BRANCH_CLEAN" > "/opt/opscode-$GIT_BRANCH_CLEAN/.branch"; fi
  sudo mv "/opt/opscode-$GIT_BRANCH_CLEAN" /opt/opscode
fi
sudo env CHEF_SOLO_COOKBOOKS="$(pwd)/cookbooks" chef-solo -c /srv/opscode-omnibus/shared/solo.rb -j /srv/opscode-omnibus/shared/solo.json
cp omnibus.rb.example omnibus.rb
rm pkg/* || true
bundle install --deployment --without development
bundle exec rake projects:private-chef:deb

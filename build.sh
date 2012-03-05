#!/bin/bash -xe
if [ ! -e /opt/opscode/.branch ]; then exit 1; fi
GIT_BRACH_CLEAN="${GIT_BRANCH//\//_}"
if [ "$GIT_BRANCH_CLEAN" != "$(cat /opt/opscode/.branch)"]
then
  mv /opt/opscode "/opt/opscode-$(cat /opt/opscode/.branch)"
  if [ ! -d "/opt/opscode-$GIT_BRANCH_CLEAN" ]; then mkdir "/opt/opscode-$GIT_BRANCH_CLEAN"; fi
  mv "/opt/opscode-$GIT_BRANCH_CLEAN" /opt/opscode
fi
sudo env CHEF_SOLO_COOKBOOKS="$(pwd)/cookbooks" chef-solo -c /srv/opscode-omnibus/shared/solo.rb -j /srv/opscode-omnibus/shared/solo.json
cp omnibus.rb.example omnibus.rb
rm pkg/* || true
bundle install --deployment --without development
bundle exec rake projects:private-chef:deb

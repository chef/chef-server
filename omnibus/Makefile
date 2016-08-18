# These targets are intended to run from a developer workstation, not
# an Omnibus build node.  If you want to muck about on the build node
# itself, please use the `build_node.mk` file instead.
#
# e.g., 'make -f build_node.mk $TARGET'

DEV_PLATFORM ?= ubuntu-1404
DEV_PROJECT=opscode-omnibus

.DEFAULT_GOAL=dev

dev:
	kitchen converge default-$(DEV_PLATFORM)

dev-login:
	kitchen login default-$(DEV_PLATFORM)

dev-destroy:
	kitchen destroy default-$(DEV_PLATFORM)

dev-suspend:
	cd .kitchen/kitchen-vagrant/default-$(DEV_PLATFORM) \
	&& vagrant suspend

dev-resume:
	cd .kitchen/kitchen-vagrant/default-$(DEV_PLATFORM) \
	&& vagrant resume

from_scratch: dev-destroy dev

update-omnibus:
	bundle update omnibus

update-omnibus-software:
	bundle update omnibus-software

update:
	bundle update omnibus omnibus-software

extract_dev_cache:
	mkdir -p $(DEV_PLATFORM)_git_cache
	cd .kitchen/kitchen-vagrant/default-$(DEV_PLATFORM) \
	&& vagrant ssh -c 'cp -R /var/cache/omnibus/cache/git_cache /home/vagrant/$(DEV_PROJECT)/$(DEV_PLATFORM)_git_cache'

deploy_dev_cache:
	cd .kitchen/kitchen-vagrant/default-$(DEV_PLATFORM) \
	&& vagrant ssh -c 'mkdir -p /var/cache/omnibus/cache && rm -rf /var/cache/omnibus/cache/* && cp -R /home/vagrant/$(DEV_PROJECT)/$(DEV_PLATFORM)_git_cache/* /var/cache/omnibus/cache'

# Remove all but the most recent installer package (and its metadata
# file) from the pkg directory.  This is used to keep the pkg
# directory small, as it is rsynced into our development Vagrant box
# (see
# https://github.com/chef/delivery/blob/master/cookbooks/delivery-dev/recipes/delivery.rb)
#
# "tail -n+2" starts the listing at the second line (i.e., we don't
# send the most recent file through to the "rm")
#
# Note that "prune" is synonymous with "prune_deb", as Ubuntu is the
# platform we currently target in development; this is the target that
# you'll use on a regular basis.  "prune_rpm" is provided for
# completeness, should you ever create CentOS installers.
prune: prune_deb

clear_pkg_cache: clear_pkg_cache_deb

clear_pkg_cache_%:
	cd .kitchen/kitchen-vagrant/kitchen-omnibus-default-$(DEV_PLATFORM) \
	&& vagrant ssh -c 'rm -v -f /var/cache/omnibus/pkg/*.$*'
	cd .kitchen/kitchen-vagrant/kitchen-omnibus-default-$(DEV_PLATFORM) \
	&& vagrant ssh -c 'rm -v -f /var/cache/omnibus/pkg/*.$*.metadata.json'

prune_%:
	ls -1t pkg/*.$* | tail -n+2 | xargs rm -v
	ls -1t pkg/*.$*.metadata.json | tail -n+2 | xargs rm -v


# Bundle install and build chef-server
dev-build: clear_pkg_cache prune
	kitchen exec default-$(DEV_PLATFORM) -c "bash -l -c 'cd chef-server/omnibus && source ~/load-omnibus-toolchain.sh && bundle install && time bundle exec omnibus build chef-server'"

install:
	gem install bundler -v 1.12.0
	bundle _1.12.0_ install

travis:
	bundle exec rake test:csc
	bundle exec rake test:veil
	bundle exec rake test:routing
	bundle exec rake test:private_chef_cookbooks

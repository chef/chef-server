bump: bump_rebars bump_bundles

bump_rebars: rebar_bookshelf rebar_chef-mover rebar_oc_bifrost rebar_oc_erchef
bump_bundles: bundle_oc-id

rebar_%:
	cd src/$* && ./rebar3 upgrade $$TARGET

bundle_%:
	cd src/$* && bundle install --no-deployment && bundle update $$TARGET

bundle_omnibus:
	cd omnibus && bundle install --no-deployment && bundle update $$TARGET

bundle_partybus:
	cd omnibus/partybus && bundle install --no-deployment && bundle update $$TARGET

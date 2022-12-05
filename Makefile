bump: bump_rebars bump_bundles

bump_rebars: rebar_bookshelf rebar_oc_bifrost rebar_oc_erchef
bump_bundles: bundle_oc-id

# After the rebar3 upgrade from 3.6.2 to 3.20.0, license scout has issues
# with transitive dependencies referenced as packages in rebar.lock files.
# So, if we do a blanket upgrade of all dependencies (rebar3 upgrade --all),
# some transitive dependencies will be referenced as packages, causing license
# scout issues. Therefore, we upgrade only the dependencies that don't pull in
# transitive dependencies.
#
# To arrive at the list of dependencies to upgrade for an app, start with the
# deps in the rebar.config, and subtract from that anything pulling in a
# transitive dependency as seen by running a `rebar3 tree` in the appropriate
# directory. Problem dependencies in a lock file will be referenced by `pkg`,
# and will have an entry in a pkg_hash and possibly a pkg_hash_ext section at
# the bottom (the rebar.lock should not contain these entries).
#
# Anything pulling in a problem dependency, and any problem dependencies themselves,
# should be upgraded by hand.
#
# It was observed that upgrading a single dependency, e.g. `./rebar3 upgrade eper`,
# rewrote the entire lockfile, removing packages.  So if a lockfile ever gets
# inadvertently polluted, it's something to try.
#
#rebar_%:
#	cd src/$* && ./rebar3 upgrade --all $$TARGET

rebar_bookshelf:
	cd src/bookshelf; \
	./rebar3 upgrade cf,chef_secrets,envy,eper,erlsom,erlware_commons,iso8601,meck,mini_s3,mixer,mochiweb,opscoderl_wm,sqerl; \
	echo "some references are pulled in as git://. rewrite to https://"; \
	sed -i '' 's/git:\/\//https:\/\//g' rebar.lock

rebar_oc_bifrost:
	cd src/oc_bifrost; \
	./rebar3 upgrade chef_secrets,edown,ej,eper,jiffy,mixer,mochiweb,opscoderl_wm,sqerl,stats_hero; \
	echo "some references are pulled in as git://. rewrite to https://"; \
	sed -i '' 's/git:\/\//https:\/\//g' rebar.lock

rebar_oc_erchef:
	cd src/oc_erchef; \
	./rebar3 upgrade cf,chef_authn,chef_secrets,darklaunch,edown,efast_xs,ej,envy,eper,folsom,folsom_graphite,ibrowse,jiffy,mini_s3,mixer,mochiweb,neotoma,opscoderl_folsom,opscoderl_httpc,opscoderl_wm,pooler,prometheus,sqerl,stats_hero,uuid; \
	echo "some references are pulled in as git://. rewrite to https://"; \
	sed -i '' 's/git:\/\//https:\/\//g' rebar.lock

bundle_%:
	cd src/$* && bundle install --no-deployment && bundle update $$TARGET

bundle_omnibus:
	cd omnibus && bundle install --no-deployment && bundle update $$TARGET

bundle_partybus:
	cd omnibus/partybus && bundle install --no-deployment && bundle update $$TARGET

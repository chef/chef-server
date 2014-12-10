DEPS=$(CURDIR)/deps
XCHECK_APPS=chef_authn oc_chef_authz chef_certgen chef_db chef_index chef_objects chef_wm \
	    oc_chef_wm sqerl

# The release branch should have a file named USE_REBAR_LOCKED
use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  rebar_config = rebar.config.lock
else
  rebar_config = rebar.config
endif
REBAR = REBAR_PARENT=$(CURDIR)/rebar.config rebar -C $(rebar_config)

all: compile

# Jenkins build target
ci: compile xcheck

compile: $(DEPS)
	@$(REBAR) compile

compile_skip:
	@$(REBAR) compile skip_deps=true

xcheck:
	@scripts/xcheck $(XCHECK_APPS)
clean:
	@$(REBAR) clean

# clean and allclean do the same thing now. Leaving allclean for now
# in case there are scripts that depend on it.
allclean:
	@$(REBAR) clean

update: compile
	@cd rel/oc_erchef;bin/oc_erchef restart

distclean: relclean
	@rm -rf deps
	@$(REBAR) clean

tags: TAGS

TAGS:
	find deps -name "*.[he]rl" -print | etags -

BUMP ?= patch
prepare_release: distclean unlocked_deps unlocked_compile update_locked_config rel
	@echo 'release prepared, bumping version'
	@$(REBAR) bump-rel-version version=$(BUMP)

unlocked_deps:
	@echo 'Fetching deps as: rebar -C rebar.config'
	@rebar -C rebar.config get-deps

# When running the prepare_release target, we have to ensure that a
# compile occurs using the unlocked rebar.config. If a dependency has
# been removed, then using the locked version that contains the stale
# dep will cause a compile error.
unlocked_compile:
	@rebar -C rebar.config compile

update_locked_config:
	@rebar lock-deps skip_deps=true

rel: rel/oc_erchef

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/* apps/*), /bin/echo -n .;rm -rf rel/oc_erchef/lib/$(shell basename $(dep))-* \
	   && ln -sf $(abspath $(dep)) rel/oc_erchef/lib;)
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

rel/oc_erchef: compile bundle
	@/bin/echo 'building OTP release package for oc_erchef'
	@/bin/echo "using rebar as: $(REBAR)"
	@$(REBAR) generate
	@/bin/echo '                             _          _  '
	@/bin/echo '                            | |        | | '
	@/bin/echo '  __   __     _   ,_    __  | |     _  | | '
	@/bin/echo ' /  \_/      |/  /  |  /    |/ \   |/  |/  '
	@/bin/echo ' \__/ \___/  |__/   |_/\___/|   |_/|__/|__/'
	@/bin/echo '                                       |\  '
	@/bin/echo '                                       |/  '

bundle:
	@cd apps/chef_objects/priv/depselector_rb; rm -rf .bundle; bundle install --deployment --path .bundle

relclean:
	@rm -rf rel/oc_erchef

$(DEPS):
	@echo "Fetching deps as: $(REBAR)"
	@$(REBAR) get-deps

.PHONY: distclean remove_lock set_lock prepare_release update_locked_config update clean compile compile_skip allclean tags relclean unlocked_deps

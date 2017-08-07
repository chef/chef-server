.DEFAULT_GOAL=all
REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 = rebar3
endif

all: $(REBAR3) elvis
	@$(REBAR3) do clean, compile, eunit, dialyzer

rel: all
	@$(REBAR3) release

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

install: $(REBAR3) distclean
	$(REBAR3) update

travis: all rel
	@$(REBAR3) ct
	@echo "Travis'd!"

# NOTE 2017-02-25: The `ct` rebar task only runs cleanly if you run
# it by itself. If, for instance, you were to try to do this:
#
#    $(REBAR3) do clean, compile eunit, dialyzer, ct
#
# The ct tests would not run and would instead produce an error trying
# to reload CTH. This appears to be a bug upstream in rebar.
ct: $(REBAR3)
	$(REBAR3) ct

elvis:
	../../scripts/elvis rock

version_clean:
	@rm -f VERSION

## echo -n only works in bash shell
SHELL=bash
REL_VERSION ?= $$(git log --oneline --decorate | grep -v -F "jenkins" | grep -F "tag: " --color=never | head -n 1 | sed  "s/.*tag: \([^,)]*\).*/\1/")-$$(git rev-parse --short HEAD)

VERSION: version_clean
	@echo -n $(REL_VERSION) > VERSION

## for Omnbibus
omnibus: $(REBAR3) distclean
	$(REBAR3) update
	$(REBAR3) do compile, release

## For dvm
DVM_HOOK = bundle
dvm_file = $(wildcard /vagrant/dvm.mk)
ifeq ($(dvm_file),/vagrant/dvm.mk)
	include /vagrant/dvm.mk
endif

bundle_clean:
	@cd apps/chef_objects/priv/depselector_rb; rm -rf .bundle

bundle:
	@echo bundling up depselector, This might take a while...
	@cd apps/chef_objects/priv/depselector_rb; bundle install --deployment --path .bundle


CHEFDK_GECODE_PATH:=/opt/chefdk/embedded/lib/ruby/gems/2.4.0/gems/dep-selector-libgecode-1.3.1/lib/dep-selector-libgecode/vendored-gecode
travis_env:
	@echo export TRAVIS=1
	@echo export USE_SYSTEM_GECODE=1
	@echo export LIBRARY_PATH=$(CHEFDK_GECODE_PATH)/lib
	@echo export LD_LIBRARY_PATH=$(CHEFDK_GECODE_PATH)/lib
	@echo export CPLUS_INCLUDE_PATH=$(CHEFDK_GECODE_PATH)/include

clean_beam:
	find _build/ -name '*.beam' -delete

distclean:
	@rm -rf _build

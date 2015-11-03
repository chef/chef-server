distclean:
	rm -rf _build

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

all: $(REBAR3)
	@$(REBAR3) do clean, compile, eunit, ct, dialyzer

rel: all
	@$(REBAR3) release

update: $(REBAR3)
	@$(REBAR3) update

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

install: $(REBAR3)

travis: update all
	@echo "Travis'd!"

ct: $(REBAR3)
	$(REBAR3) do ct --dir apps/chef_db/itest, ct --dir apps/oc_chef_authz/itest, ct --dir apps/oc_chef_wm/itest

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
dvm_file = $(wildcard /vagrant/dvm.mk)
ifeq ($(dvm_file),/vagrant/dvm.mk)
	include /vagrant/dvm.mk
endif

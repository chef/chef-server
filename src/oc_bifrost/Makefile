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

# If the user hasn't set POSTGRES_USER, default to postgres
POSTGRES_USER ?= 'postgres'

all: $(REBAR3)
	@$(REBAR3) do clean, compile, eunit, dialyzer

rel: all
	@$(REBAR3) release

pedant: pedant_setup test_pedant pedant_cleanup

pedant_setup: provision_test_db rel install_pedant start_test_rel

pedant_cleanup: stop_test_rel destroy_test_db

start_test_rel:
	PATH=$(CURDIR)/oc-bifrost-pedant/vendor/bin:$(PATH) _build/default/rel/oc_bifrost/bin/oc_bifrost start

stop_test_rel:
	_build/default/rel/oc_bifrost/bin/oc_bifrost stop

install_pedant:
	cd oc-bifrost-pedant && bundle install --binstubs=vendor/bin

test_pedant:
	@echo "Sleeping 30 seconds to allow bifrost to start"
	@sleep 30
	cd oc-bifrost-pedant && bundle exec bin/oc-bifrost-pedant --config pedant_config.rb


provision_test_db:
	psql -c 'CREATE DATABASE bifrost_test;' -U $(POSTGRES_USER)
	psql -c "CREATE USER bifrost_test_user SUPERUSER UNENCRYPTED PASSWORD 'bifrost_test_user_password';" -U $(POSTGRES_USER)
	cd schema && sqitch deploy db:pg:bifrost_test

destroy_test_db:
	psql -c "DROP USER IF EXISTS bifrost_test_user;" -U $(POSTGRES_USER)
	psql -c 'DROP DATABASE IF EXISTS bifrost_test;' -U $(POSTGRES_USER)

update: $(REBAR3)
	@$(REBAR3) update

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

install: $(REBAR3)

travis: update all pedant
	@echo "Travis'd!"

version_clean:
	@rm -f VERSION

distclean:
	rm -rf _build

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

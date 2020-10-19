.DEFAULT_GOAL=all

## For Rebar
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

# sqitch and psql use a .pgpass file pointed to by $PGPASSFILE environment variable
# hostname:port:database:username:password
# https://www.postgresql.org/docs/9.6/libpq-pgpass.html
PGPASSFILE=$(PWD)/.pgpass
DB_NAME = bookshelf
DB_USER = bookshelf
DB_PASS = test_sql_password

include Makefile.db

## TODO: put 'ct' back in here
all: $(REBAR3) elvis
	@$(REBAR3) do clean, compile, eunit, dialyzer #, xref

rel: all
	@$(REBAR3) release

update: $(REBAR3)
	@$(REBAR3) update

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

ct: $(REBAR3)
	$(REBAR3) ct

elvis:
	../../scripts/elvis rock

version_clean:
	@rm -f VERSION

distclean:
	rm -rf _build

## echo -n only works in bash shell
SHELL=bash
REL_VERSION ?= $$(git log --oneline --decorate | grep -v -F "jenkins" | grep -F "tag: " --color=never | head -n 1 | sed  "s/.*tag: \([^,)]*\).*/\1/")-$$(git rev-parse --short HEAD)

VERSION: version_clean
	@echo -n $(REL_VERSION) > VERSION

## For Travis
install: $(REBAR3)

travis: ci

ci: update all provision_test_db provision_schema ct destroy_test_db
	@echo "CI completed!"

## for Omnbibus
omnibus: $(REBAR3) distclean
	$(REBAR3) update
	$(REBAR3) do compile, release

## For dvm
PROJ = bookshelf
dvm_file = $(wildcard /vagrant/dvm.mk)
ifeq ($(dvm_file),/vagrant/dvm.mk)
	include /vagrant/dvm.mk
endif

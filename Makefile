DEPS=$(CURDIR)/deps
DIALYZER_DEPS = deps/chef_objects/ebin \
                deps/depsolver/ebin \
                deps/ej/ebin \
                deps/ejson/ebin \
                deps/epgsql/ebin \
                deps/ibrowse/ebin \
                deps/mini_s3/ebin \
                deps/mochiweb/ebin \
                deps/oauth/ebin \
                deps/pooler/ebin \
                deps/sqerl/ebin \
                deps/stats_hero/ebin
DEPS_PLT = chef_db.plt

## Set the environment variable $DB_TYPE to either mysql or pgsql
## to run the correct integration tests.
-include itest/pgsql_conf.mk

all: compile eunit dialyzer

clean:
	@rebar skip_deps=true clean

allclean:
	@rebar clean

distclean:
	@rebar skip_deps=true clean
	@rm -rf deps $(DEPS_PLT)

compile: $(DEPS)
	@rebar compile

dialyzer: $(DEPS_PLT)
	@dialyzer -Wrace_conditions -Wunderspecs --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

test: eunit

doc:
	@rebar doc skip_deps=true

tags:
	find src deps -name "*.[he]rl" -print | etags -

itest_create:
	@echo Creating integration test database
	@cd itest;./create_schema.rb pgsql create

itest_clean:
	@rm -f itest/*.beam
	@echo Dropping integration test database
	@cd itest;./create_schema.rb pgsql destroy
	@rm -rf itest/Gemfile.lock

itest: test itest_create itest_run itest_clean

itest_run:
	cd itest;erlc -I ../include -pz ../deps/chef_objects/ebin *.erl
	@erl -I include -pa deps/*/ebin -pa .eunit -pa itest -noshell -eval "eunit:test(itest)" \
	-s erlang halt -db_type pgsql

.PHONY: all clean allclean distclean compile dialyzer eunit test doc tags itest

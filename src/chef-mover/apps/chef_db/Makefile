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

all: compile eunit #dialyzer

clean:
	@rebar skip_deps=true clean

allclean:
	@rebar clean

distclean: itest_clean
	@rebar skip_deps=true clean
	@rm -rf deps $(DEPS_PLT)

compile: $(DEPS)
	@rebar compile

dialyzer: $(DEPS_PLT)
	@dialyzer --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

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

itest_clean:
	@rm -rf itest/*.beam itest/ct_logs

itest: compile
        # recompile, setting TEST define so we can play with
        # unexported functions.
	@echo "Recompiling with -DTEST ..."
	@rebar clean skip_deps=true
	@rebar compile skip_deps=true -DTEST
	@rebar skip_deps=true ct
	@echo "Review results with: open itest/ct_logs/index.html"


.PHONY: all clean allclean distclean compile eunit test doc tags itest #dialyzer

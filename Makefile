DEPS = $(CURDIR)/deps
DIALYZER_DEPS = deps/chef_authn/ebin \
                deps/oc_chef_authz/ebin \
                deps/chef_db/ebin \
                deps/chef_index/ebin \
                deps/chef_objects/ebin \
                deps/chef_wm/ebin \
                deps/depsolver/ebin \
                deps/ej/ebin \
                deps/fast_log/ebin \
                deps/mini_s3/ebin \
                deps/pooler/ebin \
                deps/sqerl/ebin \
                deps/stats_hero/ebin \
                deps/ibrowse/ebin \
                deps/webmachine/ebin \
                deps/jiffy/ebin
# note that we ommit erlware_commons from the analysis because it
# currently gives the following error:
#
# dialyzer: Analysis failed with error:
# ec_gb_trees.erl:72: Polymorphic opaque types not supported yet
# :'(
DEPS_PLT = chef_wm.plt

all: compile eunit dialyzer

clean:
	@rebar clean

compile: $(DEPS)
	@rebar compile

$(DEPS):
	@rebar get-deps

distclean:
	@rm -rf deps $(DEPS_PLT)
	@rebar skip_deps=true clean

eunit:
	@rebar skip_deps=true eunit

test: eunit

dialyzer: $(DEPS_PLT)
	@dialyzer -Wrace_conditions -Wunderspecs --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)

.PHONY: check_calls

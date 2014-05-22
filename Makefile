DEPS = $(CURDIR)/deps
# TODO: move itest deps into a common data dir (need to configure common test)
ITEST_DEPS = $(CURDIR)/itest/oc_chef_wm_containers_SUITE_data/deps
DIALYZER_DEPS = deps/chef_authn/ebin \
                deps/oc_chef_authz/ebin \
                deps/chef_db/ebin \
                deps/chef_index/ebin \
                deps/chef_objects/ebin \
                deps/chef_wm/ebin \
                deps/ej/ebin \
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
EC_SCHEMA_VERSION = 2.2.0

REBAR = REBAR_PARENT=$(CURDIR)/rebar.config rebar

all: compile eunit dialyzer

clean:
	@$(REBAR) clean

compile: $(DEPS)
	@$(REBAR) compile

$(DEPS):
	@$(REBAR) get-deps

distclean:
	@rm -rf deps $(DEPS_PLT)
	@$(REBAR) skip_deps=true clean

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit

dialyzer: $(DEPS_PLT)
	@dialyzer -Wunderspecs --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)

.PHONY: check_calls

$(ITEST_DEPS):
	mkdir -p $(ITEST_DEPS)

$(ITEST_DEPS)/enterprise-chef-server-schema: $(ITEST_DEPS)
	cd $(ITEST_DEPS); git clone git@github.com:opscode/enterprise-chef-server-schema.git; cd enterprise-chef-server-schema; git checkout $(EC_SCHEMA_VERSION); make install

itest_deps: $(ITEST_DEPS)/enterprise-chef-server-schema

itest: test itest_deps
	@erlc -pa 'deps/*/ebin' -pa ebin -I 'deps' -o itest/mocks/ itest/mocks/*.erl
	@$(REBAR) skip_deps=true ct

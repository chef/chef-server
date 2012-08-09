DEPS = $(CURDIR)/deps
DIALYZER_DEPS = deps/chef_authn/ebin \
                deps/chef_authz/ebin \
                deps/chef_db/ebin \
                deps/chef_index/ebin \
                deps/chef_objects/ebin \
                deps/ej/ebin \
                deps/fast_log/ebin \
                deps/mini_s3/ebin \
                deps/pooler/ebin \
                deps/sqerl/ebin \
                deps/stats_hero/ebin \
                deps/ibrowse/ebin \
                deps/webmachine/ebin \
                deps/ejson/ebin

DEPS_PLT = chef_wm.plt

all: compile eunit dialyzer

clean:
	@rebar clean

compile: $(DEPS) check_calls
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
	@dialyzer -Wrace_conditions -Wunderspecs --plts ~/.dialyzer_plt chef_wm.plt -r ebin

check_calls:
	@./check_calls

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)

.PHONY: check_calls

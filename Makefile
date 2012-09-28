DEPS=$(CURDIR)/deps
DIALYZER_DEPS = deps/depsolver/ebin \
                deps/ej/ebin \
                deps/jiffy/ebin \
                deps/ibrowse/ebin \
                deps/mini_s3/ebin

DEPS_PLT = chef_objects.plt

.PHONY: doc

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

doc:
	@rebar doc skip_deps=true

dialyzer: $(DEPS_PLT)
	@dialyzer -Wrace_conditions -Wunderspecs --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

test: eunit

tags:
	find src deps -name "*.[he]rl" -print | etags -

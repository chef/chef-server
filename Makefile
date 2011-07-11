DEPS = deps/couchbeam deps/ejson deps/ibrowse deps/mochiweb deps/oauth \
       deps/webmachine deps/neotoma deps/meck

all: compile

compile: $(DEPS)
	@./rebar compile

clean:
	@./rebar skip_deps=true clean
	@rm -f apps/chef_rest/src/lucene_syntax.erl

update: compile
	@cd rel/chef_api;bin/chef_api restart

allclean:
	@./rebar clean

distclean:
	@rm -rf deps
	@./rebar clean

test: eunit

eunit:
	@./rebar skip_deps=true eunit

test-common:
	@./rebar skip_deps=true eunit app=chef_common

test-rest:
	@./rebar skip_deps=true eunit app=chef_rest

dialyze: dialyzer

dialyzer:
	dialyzer -Wrace_conditions -Wspecdiffs apps/*/ebin

rel: rel/chef_api

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/*), /bin/echo -n .;rm -rf rel/chef_api/lib/$(shell basename $(dep))-* \
           && ln -sf $(abspath $(dep)) rel/chef_api/lib;)
	@$(foreach app,$(wildcard apps/*), /bin/echo -n .;rm -rf rel/chef_api/lib/$(shell basename $(app))-* \
           && ln -sf $(abspath $(app)) rel/chef_api/lib;)
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

rel/chef_api: compile
	@./rebar generate

relclean:
	@rm -rf rel/chef_api

$(DEPS):
	@./rebar get-deps
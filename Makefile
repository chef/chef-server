DEPS = deps/couchbeam deps/ejson deps/ibrowse deps/mochiweb deps/oauth \
       deps/webmachine deps/neotoma deps/meck

GRAMMARS = apps/chef_rest/src/lucene.erl apps/chef_rest/src/chef_lucene.erl
NEOTOMA = deps/neotoma/ebin/neotoma.app

all: compile

compile: $(GRAMMARS)
	@./rebar compile

compile_skip:
	@./rebar compile skip_deps=true

$(GRAMMARS): $(DEPS) $(NEOTOMA)
	@apps/chef_rest/priv/neotoma apps/chef_rest/priv/lucene.peg lucene lucene_sexp
	@apps/chef_rest/priv/neotoma apps/chef_rest/priv/lucene.peg chef_lucene lucene_txfm

$(NEOTOMA):
	@cd deps/neotoma;make

clean:
	@./rebar skip_deps=true clean
	@rm -f $(GRAMMARS)

update: compile
	@cd rel/chef_api;bin/chef_api restart

allclean:
	@./rebar clean
	@rm -f $(GRAMMARS)

distclean:
	@rm -rf deps
	@./rebar clean
	@rm -f $(GRAMMARS)

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
	@/bin/echo
	@/bin/echo Make sure opscode-chef and opscode-chef-api share the same parent directory.
	@/bin/echo Otherwise release generation will fail.
	@/bin/echo
	@./rebar generate

relclean:
	@rm -rf rel/chef_api

$(DEPS):
	@./rebar get-deps

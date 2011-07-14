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
	@cd rel/erchef;bin/erchef restart

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

rel: rel/erchef

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/*), /bin/echo -n .;rm -rf rel/erchef/lib/$(shell basename $(dep))-* \
           && ln -sf $(abspath $(dep)) rel/erchef/lib;)
	@$(foreach app,$(wildcard apps/*), /bin/echo -n .;rm -rf rel/erchef/lib/$(shell basename $(app))-* \
           && ln -sf $(abspath $(app)) rel/erchef/lib;)
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

rel/erchef: compile
	@/bin/echo 'building OTP release package for'
	@/bin/echo '                _          _  '
	@/bin/echo '               | |        | | '
	@/bin/echo ' _   ,_    __  | |     _  | | '
	@/bin/echo '|/  /  |  /    |/ \   |/  |/  '
	@/bin/echo '|__/   |_/\___/|   |_/|__/|__/'
	@/bin/echo '                          |\  '
	@/bin/echo '                          |/  '
	@/bin/echo
	@./rebar generate

relclean:
	@rm -rf rel/erchef

$(DEPS):
	@./rebar get-deps

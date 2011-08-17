DEPS = deps/couchbeam deps/ejson deps/ibrowse deps/mochiweb deps/oauth \
       deps/webmachine deps/neotoma deps/meck deps/chef_common deps/chef_rest \
       deps/emysql

GRAMMARS = deps/chef_common/src/lucene.erl deps/chef_common/src/chef_lucene.erl
NEOTOMA = deps/neotoma/ebin/neotoma.app

all: compile

compile: $(GRAMMARS)
	@./rebar compile

compile_skip:
	@./rebar compile skip_deps=true

$(GRAMMARS): $(DEPS) $(NEOTOMA)
	@deps/chef_common/priv/neotoma deps/chef_common/priv/lucene.peg lucene lucene_sexp
	@deps/chef_common/priv/neotoma deps/chef_common/priv/lucene.peg chef_lucene lucene_txfm

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

eunit: compile
	@./rebar eunit app=chef_common,chef_rest

test-common:
	@./rebar eunit app=chef_common

test-rest:
	@./rebar eunit app=chef_rest

## KAS: Temporarily disabling dialyzer target until project structure is sorted
#dialyze: dialyzer

#dialyzer:
#	dialyzer -Wrace_conditions -Wspecdiffs apps/*/ebin

rel: rel/erchef

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/*), /bin/echo -n .;rm -rf rel/erchef/lib/$(shell basename $(dep))-* \
           && ln -sf $(abspath $(dep)) rel/erchef/lib;)
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

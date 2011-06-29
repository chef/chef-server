DEPS = deps/couchbeam deps/ejson deps/ibrowse deps/mochiweb deps/oauth deps/webmachine deps/neotoma

all: compile

compile: $(DEPS)
	@./rebar compile

clean:
	@./rebar skip_deps=true clean

allclean:
	@./rebar clean

distclean:
	@rm -rf deps
	@./rebar clean

rel: rel/chef_api

devrel: rel
	@$(foreach dep,$(wildcard deps/*), rm -rf rel/chef_api/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/chef_api/lib;)
	@$(foreach app,$(wildcard apps/*), rm -rf rel/chef_api/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/chef_api/lib;)

rel/chef_api: compile
	@./rebar generate

relclean:
	@rm -rf rel/chef_api

$(DEPS):
	@./rebar get-deps
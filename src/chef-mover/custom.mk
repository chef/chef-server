SKIP_DIALYZER=true
include devvm.mk
install:
	@./rebar get-deps -C rebar.config.lock

travis: all
	 PATH=~/perl5/bin:$(PATH) $(REBARC) skip_deps=true ct

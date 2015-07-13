RELX_VERSION = 3.3.2
REL_HOOK = VERSION compile

SKIP_DIALYZER=true
include devvm.mk
install:
	@./rebar get-deps -C rebar.config.lock

travis: all
	 PATH=~/perl5/bin:$(PATH) $(REBARC) skip_deps=true ct

version_clean:
	 	@rm -f VERSION

VERSION: version_clean
ifeq ($(REL_VERSION),)
	   @echo -n "$$(git log --oneline --decorate | grep -F "tag: " --color=never | head -n 1 | sed  "s/.*tag: \([^,)]*\).*/\1/")-$$(git rev-parse --short HEAD)" > VERSION
else
	   @echo -n $(REL_VERSION) > VERSION
endif

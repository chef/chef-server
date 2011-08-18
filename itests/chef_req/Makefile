DEPS = deps/fetched-by-rebar

all: compile

compile: $(DEPS)
	@rebar compile

escript:
	@rebar escriptize skip_deps=true

compile_skip:
	@rebar compile skip_deps=true

clean:
	@rebar skip_deps=true clean

allclean:
	@rebar clean
	@rm deps/fetched-by-rebar

distclean:
	@rm -rf deps
	@rebar clean

test: eunit

eunit:
	@rebar eunit skip_deps=true

$(DEPS):
	@rebar get-deps && touch $(DEPS)

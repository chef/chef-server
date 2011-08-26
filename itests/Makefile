DEPS = deps/fetched-by-rebar
REBAR = ../rebar

all: compile

compile: $(DEPS)
	@$(REBAR) compile

compile_skip:
	@$(REBAR) compile skip_deps=true

escript:
	@$(REBAR) compile escriptize skip_deps=true

clean:
	@$(REBAR) skip_deps=true clean

allclean:
	@$(REBAR) clean
	@rm deps/fetched-by-rebar

distclean:
	@rm -rf deps
	@$(REBAR) clean

test: eunit

eunit:
	@$(REBAR) eunit skip_deps=true

$(DEPS):
	@$(REBAR) get-deps && touch $(DEPS)

DEPS = deps/automeck \
       deps/couchbeam \
       deps/ej \
       deps/ejson \
       deps/ibrowse \
       deps/meck \
       deps/mochiweb \
       deps/oauth

all: compile eunit dialyzer

clean:
	@rebar clean

distclean: clean
	@rm -rf deps

compile: $(DEPS)
	@rebar compile

dialyzer:
	@dialyzer -Wrace_conditions -Wunderspecs -r ebin

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

test: eunit

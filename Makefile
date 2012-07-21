DEPS = deps/automeck \
       deps/couchbeam \
       deps/ej \
       deps/ejson

all: compile eunit dialyzer

clean:
	@rebar clean
	@rm -rf ebin_dialyzer

distclean: clean
	@rm -rf deps

compile: $(DEPS)
	@rebar compile

dialyzer:
	@rm -rf ebin_dialyzer
	@mkdir -p ebin_dialyzer
	@cp ebin/* ebin_dialyzer
        ## added -nn which seems to speed up the analysis
	@dialyzer -Wrace_conditions -Wunderspecs -nn -r ebin_dialyzer

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

test: eunit

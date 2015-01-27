all: compile

# Clean ebin and .eunit of this project
clean:
	@rebar clean skip_deps=true

# Clean this project and all deps
allclean:
	@rebar clean

compile:
	@rebar compile

distclean:
	@rebar clean

.PHONY: all clean allclean compile distclean


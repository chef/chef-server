NAME=doest

ERL=$(shell which erl)
ERLC=$(shell which erlc)
REBAR=$(shell which rebar)

ifeq ($(REBAR),)
	$(error "Rebar not available on this system")
endif

EUNIT_DIR=$(CURDIR)/.eunit
TEST_ERLSRC=$(shell cd $(CURDIR)/test; ls | grep .erl)
TEST_OBJS=$(patsubst %.erl,$(EUNIT_DIR)/%.beam,$(TEST_ERLSRC))
OBJS=     $(patsubst %.c,$(OBJ_DIR)/%.o,$(OBJ_CSRC))

ERLPATHS=-pa .eunit -pa ebin
ERLCFLAGS=+debug_info +warnings_as_errors $(ERLPATHS)
ERLFLAGS=-sname $(NAME) $(ERLPATHS)

.SUFFIXES:
.SUFFIXES: .erl .beam
.PHONY: all compile doc clean build-plt check-plt dialyze eunit shell distclean

all: compile

compile:
	$(REBAR) compile

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean

build-plt: compile
	$(REBAR) build-plt

check-plt: compile
	$(REBAR) check-plt

dialyze:
	$(REBAR) dialyze

eunit:
	$(REBAR) eunit

$(EUNIT_DIR)/%.beam: test/%.erl
	$(ERLC) $(ERLCFLAGS) $< -o $(EUNIT_DIR)

# This fixes another brokness in rebar. Rebar does not seem to realize
# that you may want to 'just rebuild' the tests and not actually run
# them, it gives you no option for doing that.
shell: compile $(TEST_OBJS)
	@$(ERL) $(ERLFLAGS)

distclean: clean
	@rm -rvf deps/*
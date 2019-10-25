# Copyright 2012 Opscode, Inc. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

# If there is a user global plt use that. However, if there is not a user global plt
# setup the plt for creation
GLOBAL_PLT := $(wildcard $(HOME)/.dialyzer_plt)
DEPSOLVER_PLT=

ifeq ($(strip $(GLOBAL_PLT)),)
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
else
DEPSOLVER_PLT=$(GLOBAL_PLT)
endif

.PHONY: all compile doc clean eunit typer shell distclean get-deps #dialyzer typer shell distclean get-deps

all: compile eunit #dialyzer

get-deps:
	$(REBAR) get-deps
	$(REBAR) compile

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

eunit: compile
	@$(REBAR) skip_deps=true eunit

# This rule should only be invoked for the a local plt
$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
	   --apps erts kernel stdlib crypto public_key -r deps

dialyzer: $(DEPSOLVER_PLT)
	@dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src

typer:
	typer --plt $(DEPSOLVER_PLT) -r ./src

shell: compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

distclean: clean
	@rm -rvf $(CURDIR)/deps/*

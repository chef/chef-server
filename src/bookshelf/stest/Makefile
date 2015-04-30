# This Makefile written by concrete
#
# {concrete_makefile_version, 1}
#
# Use this to override concrete's default dialyzer options of
# -Wunderspecs
# DIALYZER_OPTS = ...

# List dependencies that you do NOT want to be included in the
# dialyzer PLT for the project here.  Typically, you would list a
# dependency here if it isn't spec'd well and doesn't play nice with
# dialyzer or otherwise mucks things up.
#
# DIALYZER_SKIP_DEPS = bad_dep_1 \
#                      bad_dep_2

# If you want to add dependencies to the default "all" target provided
# by concrete, add them here (along with make rules to build them if needed)
# ALL_HOOK = ...

concrete_rules_file = $(wildcard concrete.mk)
ifeq ($(concrete_rules_file),concrete.mk)
    include concrete.mk
else
    all:
	@echo "ERROR: missing concrete.mk"
	@echo "  run: concrete update"
endif

ifeq ($(wildcard bookshelf_bench.config),)
    bench:
	@echo "ERROR: missing bookshelf_bench.config"
	@echo "  run \`cp bookshelf_bench.config.example bookshelf_bench.config\`"
	@echo "  and edit as necessary."
else
    bench: compile
	@./priv/basho_bench bookshelf_bench.config
endif

view:
	@./priv/summary.r -i tests/current
	@open tests/current/summary.png

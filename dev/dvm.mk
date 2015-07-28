# Very simple make sequence that cleans, compiles and builds a release
# and does a --dev-mode relx.  In dvm, ct/eunit/dialyzer are expected to be run
# on the host.  dvm only wants to load the project.
DVM_HOOK ?=

dvm_distclean:
	@rm -rf _build

dvm_clean: dvm_distclean version_clean
	@$(REBAR3) clean

dvm_compile: VERSION
	@$(REBAR3) update
	@$(REBAR3) compile

dvm: dvm_compile $(DVM_HOOK)
	@$(REBAR3) as dev release

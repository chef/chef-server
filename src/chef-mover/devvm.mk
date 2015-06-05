# Very simple make sequence that cleans, compiles and builds a release
# and does a --dev-mode relx.  In dvm, ct/eunit/dialyzer are expected to be run
# on the host.  dvm only wants to load the project.

devvm_distclean:
	@rm -rf deps $(DEPS_PLT) #distclean

devvm_relclean: devvm_distclean
	@rm -rf $(RELX_OUTPUT_DIR) # relclean

devvm_clean: devvm_relclean
	@$(REBARC) -j 4  clean #clean

devvm_deps: devvm_clean
	@$(REBARC) -j 4 get-deps # get-deps

devvm_compile: devvm_deps
	@$(REBARC) -j 4 compile # compile

devvm_relx:
	curl -Lo relx $(RELX_URL) || wget $(RELX_URL)
	chmod a+x relx

devvm: devvm_compile devvm_relx
	@$(RELX) --dev-mode -c $(RELX_CONFIG) -o $(RELX_OUTPUT_DIR) $(RELX_OPTS) # devrel

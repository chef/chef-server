RELX_VERSION = 3.3.2
REL_HOOK = VERSION compile
CLEAN_HOOK = version_clean

SKIP_DIALYZER=true
include devvm.mk
install: $(CURDIR)/deps VERSION

travis: all

version_clean:
	 	@rm -f VERSION

## echo -n only works in bash shell
SHELL=bash
REL_VERSION ?= $$(git log --oneline --decorate | grep -v -F "jenkins" | grep -F "tag: " --color=never | head -n 1 | sed  "s/.*tag: \([^,)]*\).*/\1/")-$$(git rev-parse --short HEAD)
VERSION: version_clean
	@echo -n $(REL_VERSION) > VERSION

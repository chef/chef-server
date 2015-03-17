## Copied from opscode-dev-vm/devvm.mk
## For use with relx erlang projects

SHELL := /bin/bash

## DEV VM Targets
DEVVM_PROJ ?= $(notdir $(CURDIR))
DEVVM_ROOT ?= /srv/piab/mounts/$(DEVVM_PROJ)
DEVVM_DIR ?= $(DEVVM_ROOT)/_rel/$(PROJ)

bummer:
	@/bin/echo Bummer! If you\'re stuck reading this error message, it\'s not the end of the world!
	@/bin/echo You\'ll need to Ctrl-C twice to get out of this stuck rake command \(densoneold\)
	@/bin/echo Then \'rake ssh\' into the box and cd $(DEVVM_ROOT)
	@/bin/echo Then \'make devvm\' will get you running

devvm_stop:
	private-chef-ctl stop $(PROJ)

devvm_link:
	rm -f $(DEVVM_DIR)/sys.config
	ln -s  /var/opt/opscode/opscode-erchef/sys.config $(DEVVM_DIR)/sys.config

devvm: devvm_stop devrel devvm_link

compile_fast:
	$(REBARC) compile skip_deps=true

update: compile_fast
	@cd _rel/$(PROJ);bin/$(PROJ) restart

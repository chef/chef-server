SHELL := /bin/bash

SCENARIOS = $(shell ls -1 scenarios 2>/dev/null | tr -d /)
WORKSPACES = $(shell find scenarios/*/terraform.tfstate.d -maxdepth 1 -type d 2>/dev/null | sed 's|.*tfstate.d/||;/tfstate.d/d;')

ifneq ($(AWS_DEFAULT_PROFILE),)
	export TF_VAR_aws_profile ?= $(AWS_DEFAULT_PROFILE)
endif

ifneq ($(AWS_SSH_KEY_ID),)
	export TF_VAR_aws_ssh_key_id ?= $(AWS_SSH_KEY_ID)
endif

ifneq ($(AWS_DEPT),)
	export TF_VAR_aws_department ?= $(AWS_DEPT)
endif

ifneq ($(AWS_CONTACT),)
	export TF_VAR_aws_contact ?= $(AWS_CONTACT)
endif

ifneq ($(AWS_DEFAULT_REGION),)
	export TF_VAR_aws_region ?= $(AWS_DEFAULT_REGION)
endif

ifneq ($(AWS_DEFAULT_INSTANCE_TYPE),)
	export TF_VAR_aws_instance_type ?= $(AWS_DEFAULT_INSTANCE_TYPE)
endif

ifneq ($(PLATFORM),)
	export TF_VAR_platform ?= $(PLATFORM)
else
	export TF_VAR_platform ?= "ubuntu-18.04"
endif

ifneq ($(INSTALL_VERSION),)
	export TF_VAR_install_version_url ?= $(shell platform=$(TF_VAR_platform); for channel in unstable current stable; do mixlib-install download chef-server --url -c $$channel -a x86_64 -p $$(echo $${platform%-*} | sed 's/rhel/el/') -l $${platform\#\#*-} -v $(INSTALL_VERSION) 2>/dev/null && break; done | head -n1)
endif

ifneq ($(UPGRADE_VERSION),)
	export TF_VAR_upgrade_version_url ?= $(shell platform=$(TF_VAR_platform); for channel in unstable current stable; do mixlib-install download chef-server --url -c $$channel -a x86_64 -p $$(echo $${platform%-*} | sed 's/rhel/el/') -l $${platform\#\#*-} -v $(UPGRADE_VERSION) 2>/dev/null && break; done | head -n1)
endif

ifneq ($(ENABLE_IPV6),)
	export TF_VAR_enable_ipv6 ?= $(ENABLE_IPV6)
else
	export TF_VAR_enable_ipv6 ?= "true"
endif

ifneq ($(SCENARIO),)
	export TF_VAR_scenario ?= $(SCENARIO)
endif

ifeq ($(WORKSPACE),)
    ifeq ($(TF_VAR_enable_ipv6), "true")
        export WORKSPACE = "$(TF_VAR_scenario)-ipv6-$(TF_VAR_platform)"
    else
        export WORKSPACE = "$(TF_VAR_scenario)-ipv4-$(TF_VAR_platform)"
    endif
else
	export TF_VAR_scenario = $(shell sed 's/-ipv.-.*//' <<<$(WORKSPACE))
	export TF_VAR_enable_ipv6 = $(shell grep -q '\-ipv6-' <<<$(WORKSPACE) && echo true || echo false)
	export TF_VAR_platform = $(shell sed 's/.*-ipv.-//' <<<$(WORKSPACE))
endif

verify-aws:
ifeq ($(TF_VAR_aws_department),)
	$(error The department that owns the resources must be provided via the "AWS_DEPT" environment variable.)
endif

ifeq ($(TF_VAR_aws_contact),)
	$(error The primary contact for the resources must be provided via the "AWS_CONTACT" environment variable.)
endif

create-vpc: verify-aws
	@set -e; \
	cd aws_vpc && \
	rm -rf .terraform && \
	terraform init && \
	terraform apply -auto-approve

show-vpc: verify-aws
	@set -e; \
	cd aws_vpc && \
	terraform show

destroy-vpc: verify-aws
	@set -e; \
	cd aws_vpc && \
	terraform destroy -auto-approve && \
	rm -rf .terraform terraform.tfstate*

verify-scenario: verify-aws
ifeq ($(TF_VAR_aws_ssh_key_id),)
	$(error The ID of an AWS SSH key must be provided via the "AWS_SSH_KEY_ID" environment variable.)
endif

ifeq ($(TF_VAR_scenario),)
	$(error A valid $(SCENARIO) scenario must be provided via the "SCENARIO" environment variable.)
endif

ifeq ($(TF_VAR_install_version_url),)
	$(error A chef-server artifact couldn't be found of version $(INSTALL_VERSION) for $(TF_VAR_platform).)
endif

ifeq ($(TF_VAR_upgrade_version_url),)
	$(error A chef-server artifact couldn't be found of version $(UPGRADE_VERSION) for $(TF_VAR_platform).)
endif

init: verify-scenario
	@set -e; \
	cd scenarios/$(TF_VAR_scenario) && (test -d .terraform || terraform init) && \
	test -d "terraform.tfstate.d/$(WORKSPACE)" || terraform workspace new "$(WORKSPACE)"

plan: init
	@set -e; \
	echo -e "\n\nGenerating Plan for $(WORKSPACE)\n" && \
	cd scenarios/$(TF_VAR_scenario) && \
	terraform workspace select "$(WORKSPACE)" && \
	terraform plan

show: init
	@set -e; \
	cd scenarios/$(TF_VAR_scenario) && \
	terraform workspace select "$(WORKSPACE)" && \
	terraform show

apply: init
	@set -e; \
	echo -e "\n\nApplying $(WORKSPACE)\n" && \
	cd scenarios/$(TF_VAR_scenario) && \
	terraform workspace select "$(WORKSPACE)" && \
	terraform apply -auto-approve

apply-all:
	@set -e; \
	for scenario in $(SCENARIOS); do \
		workspace="$${scenario}-$(shell [ $(TF_VAR_enable_ipv6) = 'true' ] && echo ipv6 || echo ipv4)-$(TF_VAR_platform)"; \
		mkdir -p "scenarios/$${scenario}/terraform.tfstate.d/$$workspace"; \
		output="/tmp/$$workspace.log"; \
		echo -e "\nApplying $$workspace:\n\toutput log can be found in $${output}"; \
		WORKSPACE="$$workspace" $(MAKE) apply > "$$output" 2>&1 & \
		sleep 1; \
	done; \
	echo -en "\nWaiting for scenarios to complete ... "; \
	wait; \
	echo 'Done';

destroy: init
	@set -e; \
	echo -e "\n\nDestroying $(WORKSPACE)\n" && \
	cd scenarios/$(TF_VAR_scenario) && \
	terraform workspace select "$(WORKSPACE)" && \
	terraform destroy -auto-approve && \
	terraform workspace select default && \
	terraform workspace delete $(WORKSPACE) && \
	(rmdir terraform.tfstate.d 2>/dev/null && rm -rf .terraform)

destroy-all:
	@set -e; \
	[ -z "$(WORKSPACES)" ] && exit 0; \
	for workspace in $(WORKSPACES); do \
		echo -e "\nDestroying $$workspace"; \
		WORKSPACE=$$workspace TF_VAR_install_version_url=x TF_VAR_upgrade_version_url=x $(MAKE) destroy & \
		sleep 1; \
	done; \
	echo -en "\nWaiting for scenario cleanup to complete ... "; \
	wait; \
	echo 'Done';

clean: destroy-all

list-active-workspaces:
	@set -e; \
	tr ' ' '\n' <<<"$(WORKSPACES)"

# Lint _all_ the directories (including modules)
# terraform fmt will always exit 0, so we need to check to see if the diff
# returns anything in order to determine if lint fails
lint:
	@set -e; \
	LINT_OUTPUT=$$(terraform fmt --diff --list --write=false ./); \
	if test -n "$$LINT_OUTPUT"; then \
		echo "$$LINT_OUTPUT"; \
		exit 1; \
	fi

.PHONY: create-vpc show-vpc destroy-vpc verify-aws verify-scenario init plan show apply apply-all destroy destroy-all clean list-active-workspaces lint

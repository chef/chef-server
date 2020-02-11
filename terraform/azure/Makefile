SHELL := /bin/bash

SCENARIOS = $(shell ls -1 scenarios 2>/dev/null | tr -d /)
WORKSPACES = $(shell find scenarios/*/terraform.tfstate.d -maxdepth 1 -type d 2>/dev/null | sed 's|.*tfstate.d/||;/tfstate.d/d;')

ifneq ($(ARM_TENANT_ID),)
	export TF_VAR_arm_tenant_id ?= $(ARM_TENANT_ID)
endif

ifneq ($(ARM_SUBSCRIPTION_ID),)
	export TF_VAR_arm_subscription_id ?= $(ARM_SUBSCRIPTION_ID)
endif

ifneq ($(ARM_SSH_KEY_FILE),)
	export TF_VAR_arm_ssh_key_file ?= $(ARM_SSH_KEY_FILE)
endif

ifneq ($(ARM_DEPT),)
	export TF_VAR_arm_department ?= $(ARM_DEPT)
endif

ifneq ($(ARM_CONTACT),)
	export TF_VAR_arm_contact ?= $(ARM_CONTACT)
endif

ifneq ($(ARM_DEFAULT_LOCATION),)
	export TF_VAR_arm_location ?= $(ARM_DEFAULT_LOCATION)
endif

ifneq ($(ARM_DEFAULT_INSTANCE_TYPE),)
	export TF_VAR_arm_instance_type ?= $(ARM_DEFAULT_INSTANCE_TYPE)
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

ifneq ($(ENABLE_SMOKE_TEST),)
    export TF_VAR_enable_smoke_test ?= $(ENABLE_SMOKE_TEST)
endif

ifneq ($(ENABLE_PEDANT_TEST),)
    export TF_VAR_enable_pedant_test ?= $(ENABLE_PEDANT_TEST)
endif

ifneq ($(ENABLE_PSQL_TEST),)
    export TF_VAR_enable_psql_test ?= $(ENABLE_PSQL_TEST)
endif

ifneq ($(ENABLE_GATHER_LOGS_TEST),)
    export TF_VAR_enable_gather_logs_test ?= $(ENABLE_GATHER_LOGS_TEST)
endif

ifneq ($(ENABLE_ADDON_PUSH_JOBS),)
    export TF_VAR_enable_addon_push_jobs ?= $(ENABLE_ADDON_PUSH_JOBS)
endif

ifneq ($(ENABLE_ADDON_CHEF_MANAGE),)
    export TF_VAR_enable_addon_chef_manage ?= $(ENABLE_ADDON_CHEF_MANAGE)
endif

ifneq ($(ENABLE_CHEF_BACKEND_DEMOTION),)
    export TF_VAR_enable_chef_backend_demotion ?= $(ENABLE_CHEF_BACKEND_DEMOTION)
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

verify-arm:
ifeq ($(TF_VAR_arm_department),)
	$(error The department that owns the resources must be provided via the "ARM_DEPT" environment variable.)
endif

ifeq ($(TF_VAR_arm_contact),)
	$(error The primary contact for the resources must be provided via the "ARM_CONTACT" environment variable.)
endif

create-resource-group: verify-arm
	@set -e; \
	cd modules/arm_resource_group && \
	rm -rf .terraform && \
	terraform init && \
	terraform apply -auto-approve

show-resource-group: verify-arm
	@set -e; \
	cd modules/arm_resource_group && \
	terraform show

destroy-resource-group: verify-arm
	@set -e; \
	cd modules/arm_resource_group && \
	terraform destroy -auto-approve && \
	rm -rf .terraform terraform.tfstate*

verify-scenario: verify-arm
ifeq ($(TF_VAR_arm_ssh_key_file),)
	$(error The file system path for a SSH public key must be provided via the "ARM_SSH_KEY_FILE" environment variable.)
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

.PHONY: create-resource-group show-resource-group destroy-resource-group verify-arm verify-scenario init plan show apply apply-all destroy destroy-all clean list-active-workspaces lint

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

ifneq ($(UNSTABLE_VERSION),)
	export TF_VAR_unstable_version ?= $(UNSTABLE_VERSION)
endif

ifneq ($(STABLE_VERSION),)
	export TF_VAR_stable_version ?= $(STABLE_VERSION)
endif

ifneq ($(ENABLE_IPV6),)
	export TF_VAR_enable_ipv6 ?= $(ENABLE_IPV6)
else
	export TF_VAR_enable_ipv6 ?= "true"
endif

ifneq ($(SCENARIO),)
	export TF_VAR_scenario ?= $(SCENARIO)
endif

verify-aws:
ifeq ($(TF_VAR_aws_department),)
	$(error The department that owns the resources must be provided via the "AWS_DEPT" environment variable.)
endif

ifeq ($(TF_VAR_aws_contact),)
	$(error The primary contact for the resources must be provided via the "AWS_CONTACT" environment variable.)
endif

create-vpc: verify-aws
	cd aws_vpc && \
	rm -rf .terraform && \
	terraform init && \
	terraform apply -auto-approve

show-vpc: verify-aws
	cd aws_vpc && \
	terraform show

destroy-vpc: verify-aws
	cd aws_vpc && \
	terraform destroy -auto-approve && \
	rm -rf .terraform terraform.tfstate*

check-scenario: verify-aws
ifeq ($(TF_VAR_aws_ssh_key_id),)
	$(error The ID of an AWS SSH key must be provided via the "AWS_SSH_KEY_ID" environment variable.)
endif

ifeq ($(TF_VAR_scenario),)
	$(error A valid $(SCENARIO) scenario must be provided via the "SCENARIO" environment variable.)
endif

ifeq ($(TF_VAR_unstable_version),)
	$(error An unstable version of chef-server must be provided via the "UNSTABLE_VERSION" environment variable.)
endif

ifeq ($(TF_VAR_stable_version),)
	$(error A stable version of chef-server must be provided via the "STABLE_VERSION" environment variable.)
endif

init: check-scenario
	cd scenarios/$(SCENARIO) && test -d .terraform || terraform init

plan: init
	cd scenarios/$(SCENARIO) && terraform plan

show: init
	cd scenarios/$(SCENARIO) && terraform show

apply: init
	cd scenarios/$(SCENARIO) && terraform apply -auto-approve

destroy: init
	cd scenarios/$(SCENARIO) && \
	terraform destroy -auto-approve && \
	rm -rf .terraform terraform.tfstate*

list-active-workspaces:
	@set -e; \
	tr ' ' '\n' <<<"$(WORKSPACES)"

# Lint _all_ the directories (including modules)
# terraform fmt will always exit 0, so we need to check to see if the diff
# returns anything in order to determine if lint fails
lint:
	set -e; \
	LINT_OUTPUT=$$(terraform fmt --diff --list --write=false ./); \
	if test -n "$$LINT_OUTPUT"; then \
		echo "$$LINT_OUTPUT"; \
		exit 1; \
	fi

.PHONY: create-vpc show-vpc destroy-vpc verify-aws verify-scenario init plan show apply apply-all destroy destroy-all clean list-active-workspaces lint

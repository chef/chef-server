# Existing environments: dev, acceptance, union, rehearsal, delivered.
TF_ENVIRONMENT ?= dev
TF_DIRECTORY ?= acceptance
TF_STATE ?= $(TF_ENVIRONMENT).tfstate

ifeq ($(TF_ENVIRONMENT), dev)
	# We have 4 isolated dev environments
	export TF_STATE=dev-$(TF_ENVIRONMENT_INDEX).tfstate
	export TF_VAR_environment=dev-$(TF_ENVIRONMENT_INDEX)

	export TF_VAR_chef_environment ?= $(TF_ENVIRONMENT)
	export TF_VAR_chef_server_url ?= https://chef-server.chef.co/organizations/chef
	export TF_VAR_chef_user_name ?= $(USER)
	export TF_VAR_chef_user_key ?= $(HOME)/.chef/$(USER).pem
	export TF_VAR_aws_key_name ?= $(USER)
	export TF_VAR_aws_private_key ?= $(HOME)/.ssh/id_rsa
else ifeq ($(TF_ENVIRONMENT), acceptance)
	export TF_DIRECTORY = acceptance
endif

$(TF_VAR_aws_private_key):
	$(error Oops, looks like you are missing your AWS private key \($(TF_VAR_aws_private_key)\). Please make sure the file exists, has the correct permissions \(0600\), and is not password protected.)

$(TF_VAR_chef_user_key):
	$(error Oops, looks like you are missing your Chef user key \($(TF_VAR_chef_user_key)\). Please make sure the file exists. If you need to regenerate it, please visit $(TF_VAR_chef_server_url).)

init: $(TF_VAR_aws_private_key) $(TF_VAR_chef_user_key)
	$(info ===============================================================)
ifeq ($(TF_ENVIRONMENT), dev)
ifeq ($(TF_ENVIRONMENT_INDEX),)
	$(error The dev environment requires an index! Please export $$TF_ENVIRONMENT_INDEX and try again.)
endif
	$(info Environment........$(TF_VAR_environment))
else
	$(info Environment........$(TF_ENVIRONMENT))
endif
	$(info Terraform State....$(TF_STATE))
	$(info Chef Environment...$(TF_VAR_chef_environment))
	$(info Chef Server URL....$(TF_VAR_chef_server_url))
	$(info Chef User Name.....$(TF_VAR_chef_user_name))
	$(info Chef User Key......$(TF_VAR_chef_user_key))
	$(info AWS Keypair Name...$(TF_VAR_aws_key_name))
	$(info AWS Private Key....$(TF_VAR_aws_private_key))
	$(info ===============================================================)
	$(info )

remote-state: init
	# We make sure that existing local state is deleted before configuring
	# the remote state. If it is not removed state gets contaminated between
	# environments.

	cd $(TF_DIRECTORY) && \
	rm -rf .terraform && \
	terraform remote config \
		-backend=s3 \
		-backend-config="profile=chef-cd" \
		-backend-config="bucket=chef-cd-terraform-state" \
		-backend-config="key=chef-server/$(TF_STATE)" \
		-backend-config="region=us-west-2" && \
	terraform get

plan: remote-state
	cd $(TF_DIRECTORY) && terraform plan -var-file=vars/$(TF_ENVIRONMENT).tfvars

apply: remote-state
	cd $(TF_DIRECTORY) && terraform apply -var-file=vars/$(TF_ENVIRONMENT).tfvars

destroy: remote-state
ifeq ($(TF_ENVIRONMENT), dev)
	cd $(TF_DIRECTORY) && terraform destroy -var-file=vars/$(TF_ENVIRONMENT).tfvars
else
	@echo "The destroy target is only supported for dev environments."
endif

show: remote-state
	cd $(TF_DIRECTORY) && terraform show

output: remote-state
	cd $(TF_DIRECTORY) && terraform output

console: remote-state
	cd $(TF_DIRECTORY) && terraform console

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

.PHONY: init remote-state plan apply destroy show output console lint

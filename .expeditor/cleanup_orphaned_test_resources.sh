#!/usr/bin/env bash

set -uo pipefail

terminate_instance() {
  local instance_id
  instance_id="$1"

  if aws --profile chef-cd ec2 terminate-instances --instance-ids "$instance_id"; then
    echo "Terminated instance $instance_id"
  else
    echo "Failed to terminate instance $instance_id"
  fi
}

terminate_instances() {
  while read -r instance_id; do
    terminate_instance "$instance_id"
  done
}

echo "--- Setting up Azure credentials"
export VAULT_UTIL_SECRETS="{\"ARM_TENANT_ID\":{\"account\":\"azure/engineering-dev-test\",\"field\":\"tenant_id\"},\"ARM_CLIENT_ID\":{\"account\":\"azure/engineering-dev-test\",\"field\":\"client_id\"},\"ARM_CLIENT_SECRET\":{\"account\":\"azure/engineering-dev-test\",\"field\":\"client_secret\"}}"
. <(vault-util fetch-secret-env)

# this allows time for the new service-principal to become available
sleep 30

az login --service-principal --tenant "$ARM_TENANT_ID" --username "$ARM_CLIENT_ID" --password "$ARM_CLIENT_SECRET"

echo "--- Deleting Azure chef-eng-team-chef_server-test resource groups"
az group list --query "[?ends_with(name, 'chef-eng-team-chef_server-test')].name" --output tsv | xargs -n1 -t -I% az group delete -y --no-wait --name "%"

echo "--- Deleting AWS instances older than 12 hours"
aws --profile chef-cd ec2 describe-instances --output text --no-paginate --query "Reservations[?Instances[?(Tags[?Key == 'Name' && ends_with(Value, 'chef_server-test')]) && (LaunchTime < '$(date --utc +%Y-%m-%dT%H:%M:%S.000Z --date=-12hours)')]].[Instances|[0].InstanceId]" | terminate_instances

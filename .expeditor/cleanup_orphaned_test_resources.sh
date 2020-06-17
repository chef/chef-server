#!/usr/bin/env bash

set -euo pipefail

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

echo "--- Deleting AWS instances older than 12 hours"
aws --profile chef-cd ec2 describe-instances --output text --no-paginate --query "Reservations[?Instances[?(Tags[?Key == 'Name' && ends_with(Value, 'chef_server-test')]) && (LaunchTime < '$(date --utc +%Y-%m-%dT%H:%M:%S.000Z --date=-12hours)')]].[Instances|[0].InstanceId]" | terminate_instances

#!/bin/bash

set -evx

# Connect to ACC
connect_to_acc

# Add the delivery remote for the products/chef-server master pipeline
add_delivery_remote products chef-server

# Checkout the a new feature branch based on delivery/master
git fetch --all --tags
git checkout -b deploy-${VERSION} delivery/master

# Merge the changes from the Github ${VERSION} tag into the feature branch
# This will create a merge commit that can be used for the Workflow patchset
git merge --no-ff --no-edit refs/tags/${VERSION}

# WIP - enable debug output from delivery CLI
export RUST_LOG=debug

# Submit the change to Workflow and grab the change id
change_id=$(delivery_review)

# Wait a few moments for the change to be ready to approve
wait_for_change_to_be_ready_to_approve products chef-server ${change_id}

# Approve change
approve_change products chef-server ${change_id}

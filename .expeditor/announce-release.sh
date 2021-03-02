#!/bin/bash

set -exou pipefail

# Download the release-notes for our specific build
curl -o release-notes.md "https://packages.chef.io/release-notes/${EXPEDITOR_PRODUCT_KEY}/${EXPEDITOR_VERSION}.md"

topic_title="Chef Infra Server $EXPEDITOR_VERSION Released!"
topic_body=$(cat <<EOH
We are delighted to announce the availability of version $EXPEDITOR_VERSION of Chef Infra Server.
$(cat release-notes.md)

---
## Get the Build

You can download binaries directly from [downloads.chef.io](https://downloads.chef.io/tools/infra-server/stable?v=$EXPEDITOR_VERSION).
EOH
)

# Use Expeditor's built in Bash helper to post our message: https://git.io/JvxPm
post_discourse_release_announcement "$topic_title" "$topic_body"

# Cleanup
rm release-notes.md

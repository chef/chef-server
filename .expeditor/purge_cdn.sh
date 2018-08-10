#!/bin/bash

set -eou pipefail

TARGET_CHANNEL="${CHANNEL:-unstable}"

echo "Purging '${TARGET_CHANNEL}/chef-server/latest' Surrogate Key group from Fastly"
curl -X POST -H "Fastly-Key: $FASTLY_API_TOKEN" https://api.fastly.com/service/1ga2Kt6KclvVvCeUYJ3MRp/purge/${TARGET_CHANNEL}/chef-server/latest

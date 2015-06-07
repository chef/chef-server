#!/usr/bin/env bash

mkdir -p ~/.realcache/cacheroot
mkdir -p ~/.realcache/archives
export CACHE_NAME=`basename $1`
source $TRAVIS_BUILD_DIR/scripts/s3cache.sh

# Let's snag the cache if we can
s3cache_fetch_branch_cache $TRAVIS_BRANCH
if [ "$FETCH_CACHE_RESPONSE" != "200" ]; then
    echo "Cache not found for $TRAVIS_BRANCH, falling back to master."
    s3cache_fetch_branch_cache "master"
    if [ "$FETCH_CACHE_RESPONSE" != "200" ]; then
      echo "Oops,nothing for master! Don't worry, I got this."
    fi
fi
if [ -f $OLD_CACHE_FILE ]; then
  echo "Extracting existing cache"
  cd $CACHE_DIR
  tar xf $OLD_CACHE_FILE

  # We want everything!
  # TODO: Do we really? Seems that if items are removed from the
  # cache we should filter them out ...
  rsync $CACHE_DIR/ $HOME/
fi



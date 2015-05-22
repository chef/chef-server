#!/usr/bin/env bash

export CACHE_NAME=`basename $1`
source $TRAVIS_BUILD_DIR/scripts/s3cache.sh
mkdir -p $CACHE_DIR
mkdir -p $CACHE_ARCHIVE_DIR

# Let's snag the cache if we can
s3cache_fetch_branch_cache $PR_SLUG
if [ "$FETCH_CACHE_RESPONSE" != "200" ]; then
    echo "Cache not found for $PR_SLUG, falling back to $TRAVIS_BRANCH"
    rm $OLD_CACHE_FILE
    s3cache_fetch_branch_cache $TRAVIS_BRANCH
    if [ "$FETCH_CACHE_RESPONSE" != "200" ]; then
      echo "Oops,nothing for master! Don't worry, I got this."
      rm $OLD_CACHE_FILE
    fi
fi
if [ -f $OLD_CACHE_FILE ]; then
  echo "Extracting existing cache"
  cd $CACHE_DIR
  tar xf $OLD_CACHE_FILE

  # We want everything!
  # TODO: Do we really? Seems that if items are removed from the
  # cache we should filter them out ...
  rsync -azr --stats $CACHE_DIR/ $HOME
fi



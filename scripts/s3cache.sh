# Expects:
# -  $TRAVIS_BRANCH
# -  $TRAVIS_REPO_SLUG
# -  $TRAVIS_PULL_REQUEST
# -  $CACHE_NAME
# -  $AWS_UPLOAD_KEY
# -  $AWS_UPLOAD_SECRET


BUCKET=chef-travis-ci-cache
BASE_URL="https://$BUCKET.s3.amazonaws.com"
CACHE_DIR=$HOME/.realcache/cacheroot
CACHE_ARCHIVE_DIR=$HOME/.realcache/archives
OLD_CACHE_FILE=$CACHE_ARCHIVE_DIR/cache.tar.gz
NEW_CACHE_FILE=$CACHE_ARCHIVE_DIR/new-cache.tar.gz
PR_SLUG=$TRAVIS_BRANCH-$TRAVIS_PULL_REQUEST

function cache_uri_for_branch {
  CACHE_URI=$BASE_URL/$TRAVIS_REPO_SLUG/$1/$CACHE_NAME.tar.gz
}

# Attempts to fetch the cache for the named branch($1)
# and sets FETCH_CACHE_RESPONSE to the http response code.
# Output is saved to OLD_CACHE_FILE.
function s3cache_fetch_branch_cache {
  cache_uri_for_branch $1
  echo "Fetching from $CACHE_URI"
  FETCH_CACHE_RESPONSE=$(curl --write-out %{http_code} --silent --output $OLD_CACHE_FILE $CACHE_URI)
}

function s3cache_put {
  targetname=$TRAVIS_REPO_SLUG/$PR_SLUG/$CACHE_NAME.tar.gz
  date=$(date +"%a, %d %b %Y %T %z")
  acl="x-amz-acl:public-read"
  content_type='application/x-compressed-tar'
  string="PUT\n\n$content_type\n$date\n$acl\n/$BUCKET/$targetname"
  signature=$(echo -en "${string}" | openssl sha1 -hmac "${AWS_UPLOAD_SECRET}" -binary | base64)
  curl -X PUT -T $NEW_CACHE_FILE \
    -H "Host: $BUCKET.s3.amazonaws.com" \
    -H "Date: $date" \
    -H "Content-Type: $content_type" \
    -H "$acl" \
    -H "Authorization: AWS ${AWS_UPLOAD_KEY}:$signature" \
    "https://$BUCKET.s3.amazonaws.com/$targetname" \
    -o /tmp/results.xml
  cat /tmp/results.xml
}



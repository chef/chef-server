#!/usr/bin/env bash
find $1 -type f -not -name "*.md" -exec file {} \;  | grep "UTF-8"

# Exit code zero means that results were found that come up as UTF-8
if [ "$?" == "0" ]; then
  echo ""
  echo "^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^"
  echo ""
  echo "Please correct the file(s) above which contain non-ASCII UTF-8 characters."
  echo ""
  exit 1
fi

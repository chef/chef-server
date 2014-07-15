#!/bin/sh

# I would prefer to use git diff --cached -S":focus" here
# but -S will return a positive result if items are either
# removed or added. We don't want to stop someone removing a :focus,
# so we'll rely on the diff output format instead
if git diff --cached|egrep "^\\+.*:focus" > /dev/null 2>&1
then
  echo "Remove :focus tag(s) before committing."
  exit 1
fi
exit 0


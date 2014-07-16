#!/bin/sh

# We'll use the git built-in only to perform this diff, beacuse
# can't rely on external tools such as regex greps. This means we can't
# determine if the word :focus has been added or removed. The best we can
# do is include some instruction on how to bypass when it's appropriate.
git --no-pager diff --cached -S":focus"
if [ $? -ne 0 ]
then
  echo "**"
  echo "** Remove :focus tag(s) before committing, or use git commit -n"
  echo "** if this commit is either removing such a tag, or is adding "
  echo "** it for documentation purposes."
  echo "**"
  exit 1
fi
exit 0


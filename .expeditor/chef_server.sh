#!/bin/bash -e

#===============================================================================
#Downloading the automate repo
#===============================================================================
export DEFAULT_AUTOMATE_REPO="https://github.com/chef/automate-private.git"

if [ -z "$AUTOMATE_REPO" ];
then
	AUTOMATE_REPO="$DEFAULT_AUTOMATE_REPO"
fi

git clone $AUTOMATE_REPO

repo_dir=`basename -s .git $AUTOMATE_REPO`

cd ${repo_dir}

if [ "${AUTOMATE_BRANCH}" != "" ]
then
  git checkout "${AUTOMATE_BRANCH}"
fi

#===============================================================================
chmod +x ./integration/tests/chef_server.sh
#running the chef_server.sh script from the automate repo
integration/run_test integration/tests/chef_server.sh
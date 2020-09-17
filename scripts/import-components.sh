#!/bin/bash
#
# import-components: A one-time script to document the import
# procedure of the chef-server component repositories
#
set -e

PROJECTS="bookshelf chef-mover chef-server-bootstrap oc-chef-pedant oc-id oc_bifrost oc_erchef"

# Enable dotglob and extglob to enable generating git mv commands more
# easily
shopt -s dotglob extglob

add_remote() {
    project=$1
    git remote add "$project" "https://github.com/chef/${project}.git"

}

checkout_project() {
    project=$1
    git fetch "${project}"
    git checkout -b "${project}-master" "$project/master"
}

# This assumes you are checked out into the component project branch
#
# An explicit git-mv at some point in the history (either before or
# after the merge) appears to be the only way to get `git log
# --follow` to work on the resulting repository correctly
move_files() {
    proj=$1
    mkdir $1
    git mv !(${proj}|.git) ${proj}/
    git commit -m "Move ${proj} files into ${proj}/"
}

# This assumes you are on master
#
# Move many of the components into src/ We don't move them here
# straight-away because many of the repositories already have a src/
# directory, which gets pretty messy without the intermediate move
move_files_2() {
    # Don't move oc-chef-pedant
    mkdir src/
    for i in ${PROJECTS//oc-chef-pedant}; do
        git mv $i/ src/${i}/
    done
    git commit -m "Move components into src/"
}

import_component() {
    proj=$1
    checkout_project $proj
    move_files $proj
    git checkout master
    git merge "${proj}-master" -m "Import $proj into chef-server repository"
}


# Opscode-omnibus is a bit different from the PROJECTS since the
# dirname will be different so we do this one by hand
add_remote "opscode-omnibus"
checkout_project "opscode-omnibus"
mkdir omnibus
git mv !(omnibus|.git) omnibus/
git commit -m "Move opscode-omnibus files into omnibus/"
git checkout master
git merge opscode-omnibus-master -m "Import opscode-omnibus into chef-server repository"

for proj in $PROJECTS; do
    echo "Adding remote for $proj"
    add_remote $proj
done

for proj in $PROJECTS; do
    import_component $proj
done

move_files_2

# Disable Dotglob
shopt -u dotglob

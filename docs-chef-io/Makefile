# we use pushd/popd here, and /bin/sh of our chefes/buildkite image is not bash
# so we have to override the default shell here
SHELL=bash

preview_netlify: chef_web_docs
	cp -R content/* chef-web-docs/_vendor/github.com/chef/chef-server/docs-chef-io/content
	cp -R static/* chef-web-docs/_vendor/github.com/chef/chef-server/docs-chef-io/static
	pushd chef-web-docs && make assets; hugo --buildFuture --gc --minify && popd

# serve: chef_web_docs
# 	echo "replace github.com/chef/chef-server/docs-chef-io => ../" >> chef-web-docs/go.mod
# 	pushd chef-web-docs && make assets; hugo server --buildDrafts --buildFuture --noHTTPCache --ignoreVendorPaths "github.com/chef/automate/**" && popd

serve: chef_web_docs
	cp -R content/* chef-web-docs/content
	cp -R static/* chef-web-docs/static
	cp config.toml chef-web-docs/config.toml
	pushd chef-web-docs && make assets; hugo server --buildDrafts --noHTTPCache --buildFuture && popd

chef_web_docs:
	if [ -d "chef-web-docs/" ]; then \
		pushd chef-web-docs && git reset HEAD --hard; git pull origin master && popd; \
	else \
		git clone https://github.com/chef/chef-web-docs.git; \
	fi

clean_all:
	rm -rf chef-web-docs


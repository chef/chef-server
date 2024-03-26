# we use pushd/popd here, and /bin/sh of our chefes/buildkite image is not bash
# so we have to override the default shell here
SHELL=bash

preview_netlify: chef_web_docs
	rm -rf chef-web-docs/_vendor/github.com/chef/chef-server/docs-chef-io/*
	cp -R content chef-web-docs/_vendor/github.com/chef/chef-server/docs-chef-io
	cp -R static chef-web-docs/_vendor/github.com/chef/chef-server/docs-chef-io
	cp -R config.toml chef-web-docs/_vendor/github.com/chef/chef-server/docs-chef-io/config.toml

	if [ -d "layouts" ]; then \
		cp -R layouts chef-web-docs/_vendor/github.com/chef/chef-server/docs-chef-io; \
	fi

	pushd chef-web-docs && bash ./scripts/netlify-deploy-preview.sh && popd

serve: chef_web_docs
	printf "go 1.22\n\nuse .\nuse ../" > chef-web-docs/hugo.work
	pushd chef-web-docs && make bundle && HUGO_MODULE_WORKSPACE=hugo.work hugo server --buildDrafts --buildFuture --noHTTPCache --ignoreVendorPaths "github.com/chef/chef-server/docs-chef-io" && popd

chef_web_docs:
	if [ -d "chef-web-docs/" ]; then \
		pushd chef-web-docs && git reset HEAD --hard; git clean -fd; git pull --ff-only origin main; rm -rf public && popd; \
	else \
		git clone https://github.com/chef/chef-web-docs.git; \
	fi

clean_all:
	rm -rf chef-web-docs


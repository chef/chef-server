name "enterprise-chef-server-schema"
version "2.2.1"

# Note that if you need changes that came in for the base schema (the
# one shared with Open Source Chef Server), you'll need to explicitly
# manage that dependency's version.  There isn't currently any support
# for that baked into enterprise-chef-server-schema, since it's just
# held together by Makefiles at the moment (i.e., there's no Bundler
# or Rebar to nicely manage dependencies).

dependency "chef-server-schema"

source :git => "git@github.com:opscode/enterprise-chef-server-schema.git"

build do
  command "mkdir -p #{install_dir}/embedded/service/#{name}"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/#{name}/"
end

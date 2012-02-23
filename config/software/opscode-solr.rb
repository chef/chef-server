name "opscode-solr"
version "0.10.8"

dependencies ["rsync"]

source :git => "git://github.com/opscode/chef"

relative_path "chef"

service_dir = "/opt/opscode/embedded/service/opscode-solr"

build do
  # TODO: when we upgrade solr to > 1.4.1, we should think about
  # building it from source

  command "mkdir -p #{service_dir}"

  # copy solr jetty
  command "mkdir -p #{service_dir}/jetty"
  command "/opt/opscode/embedded/bin/rsync -a chef-solr/solr/solr-jetty/ #{service_dir}/jetty/"

  # copy solr home
  command "mkdir -p #{service_dir}/home"
  command "/opt/opscode/embedded/bin/rsync chef-solr/solr/solr-home/ #{service_dir}/home/"
end

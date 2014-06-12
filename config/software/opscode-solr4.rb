name "opscode-solr4"
default_version "4.5.1"

dependency "rsync"
dependency "server-jre"

source :url => "http://www.dsgnwrld.com/am/lucene/solr/4.5.1/solr-#{version}.tgz",
       :md5 => "7c8c9fbbade5c119288b06c501fa46b2"

relative_path "solr-#{version}"

service_dir = "#{install_dir}/embedded/service/opscode-solr4"

build do
  command "mkdir -p #{service_dir}"

  # copy over the licenses
  command "#{install_dir}/embedded/bin/rsync -a licenses LICENSE.txt NOTICE.txt #{service_dir}/"

  # clean up solr jetty and copy
  #
  # we'll remove all of the examples that ship with solr and build our own Solr home
  # with the chef recipes
  command "mkdir -p #{service_dir}/jetty"
  command "rm -rf example/example* example/multicore example/solr"
  command "#{install_dir}/embedded/bin/rsync -a example/ #{service_dir}/jetty/"
end

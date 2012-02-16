
if node['private_chef']['mysql']['install_libs'] 
  case node["platform"]
  when "ubuntu"
    package "libmysqlclient-dev"
  when "centos","redhat","scientific"
    package "libmysql-devel"
  end
end

unless File.exists?("/var/opt/opscode/mysql-bootstrap")

  bundles = {
    "mixlib-authorization" => false,
    "opscode-account" => "test",
    "opscode-chef" => "integration_test dev",
    "opscode-expander" => false,
    "opscode-test" => "dev",
    "opscode-webui" => "integration_test dev"
  }

  bundles.each do |name, without_list| 
    execute "sed -i -e 's/mysql://g' /opt/opscode/embedded/service/#{name}/.bundle/config"
    execute "sed -i -e 's/:mysql//g' /opt/opscode/embedded/service/#{name}/.bundle/config"
    execute "sed -i -e 's/mysql//g' /opt/opscode/embedded/service/#{name}/.bundle/config"

    to_run = "/opt/opscode/embedded/bin/bundle update mysql2"

    execute "fix_bundle_#{name}" do
      command to_run
      environment({
        "PATH" => "/opt/opscode/embedded/bin:/opt/opscode/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin",
        "LD_FLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
        "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
      })
      cwd "/opt/opscode/embedded/service/#{name}"
    end
  end

  if node["private_chef"]["mysql"]["destructive_migrate"] && node['private_chef']['bootstrap']['enable']
    execute "migrate_database_1" do
      command "/opt/opscode/embedded/bin/bundle exec sequel -m db/migrate mysql2://#{node['private_chef']['mysql']['sql_user']}:#{node['private_chef']['mysql']['sql_password']}@localhost/opscode_chef -M 0"
      cwd "/opt/opscode/embedded/service/mixlib-authorization"
    end

    execute "migrate_database_2" do
      command "/opt/opscode/embedded/bin/bundle exec sequel -m db/migrate mysql2://#{node['private_chef']['mysql']['sql_user']}:#{node['private_chef']['mysql']['sql_password']}@localhost/opscode_chef"
      cwd "/opt/opscode/embedded/service/mixlib-authorization"
    end
  end

  file "/var/opt/opscode/mysql-bootstrap" 
end

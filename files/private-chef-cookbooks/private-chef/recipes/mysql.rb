
if node['private_chef']['mysql']['install_libs'] 
  case node["platform"]
  when "ubuntu"
    package "libmysqlclient-dev"
  when "centos","redhat","scientific"
    package "libmysql-devel"
  end
end

bundles = {
  "mixlib-authorization" => false,
  "opscode-account" => "test",
  "opscode-chef" => "integration_test dev",
  "opscode-expander" => false,
  "opscode-test" => "dev",
  "opscode-webui" => "integration_test dev"
}

execute "/opt/opscode/embedded/bin/gem unpack /opt/opscode/embedded/service/gem/ruby/1.9.1/cache/mysql2-#{node['private_chef']['mysql']['mysql2_version']}.gem" do
  cwd "/opt/opscode/embedded/service/gem/ruby/1.9.1/gems"
  not_if { File.directory?("/opt/opscode/embedded/service/gem/ruby/1.9.1/gems/mysql2-#{node['private_chef']['mysql']['mysql2_version']}") }
end

execute "sed -i -e 's/s.files = `git ls-files`/s.files = `find . -type f`/' /opt/opscode/embedded/service/gem/ruby/1.9.1/gems/mysql2-#{node['private_chef']['mysql']['mysql2_version']}/mysql2.gemspec"
execute "sed -i -e 's/s.test_files = `git ls-files spec examples`/s.test_files = `find spec examples -type f`/' /opt/opscode/embedded/service/gem/ruby/1.9.1/gems/mysql2-#{node['private_chef']['mysql']['mysql2_version']}/mysql2.gemspec"

execute "compile mysql2" do
  command "/opt/opscode/embedded/bin/rake compile" 
  cwd "/opt/opscode/embedded/service/gem/ruby/1.9.1/gems/mysql2-#{node['private_chef']['mysql']['mysql2_version']}"
  not_if { File.directory?("/opt/opscode/embedded/service/gem/ruby/1.9.1/gems/mysql2-#{node['private_chef']['mysql']['mysql2_version']}/lib/mysql2/mysql2.so") }
end

ruby_block "create mysql2 gemspec" do
  block do
    gemspec = Gem::Specification.load("/opt/opscode/embedded/service/gem/ruby/1.9.1/gems/mysql2-#{node['private_chef']['mysql']['mysql2_version']}/mysql2.gemspec").to_ruby_for_cache
    File.open("/opt/opscode/embedded/service/gem/ruby/1.9.1/specifications/mysql2-#{node['private_chef']['mysql']['mysql2_version']}.gemspec", "w") do |spec_file|
      spec_file.print gemspec
    end
  end
  not_if { File.exists?("/opt/opscode/embedded/service/gem/ruby/1.9.1/specifications/mysql2-#{node['private_chef']['mysql']['mysql2_version']}.gemspec") }
end

bundles.each do |name, without_list| 
  execute "sed -i -e 's/mysql://g' /opt/opscode/embedded/service/#{name}/.bundle/config"
  execute "sed -i -e 's/:mysql//g' /opt/opscode/embedded/service/#{name}/.bundle/config"
  execute "sed -i -e 's/mysql//g' /opt/opscode/embedded/service/#{name}/.bundle/config"
end

if !File.exists?("/var/opt/opscode/mysql-bootstrap")
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

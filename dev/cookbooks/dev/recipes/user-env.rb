# User-specific setup
directory "/home/vagrant/.ssh" do
  action :create
  owner "vagrant"
  group "vagrant"
  mode "0700"
end

template "/home/vagrant/.ssh/ssh_config" do
  source "ssh_config"
  action :create
  owner "vagrant"
  user "vagrant"
  mode "600"
end

[["root", "/root"],["vagrant", "/home/vagrant"]].each do |user|
  who, homedir = user
  directory "#{homedir}/bin" do
    action :create
    owner who
    user who
  end

  directory "#{homedir}/.chef" do
    action :create
    owner who
    user who
  end

  file "#{homedir}/.erlang" do
    content "code:load_abs(\"#{homedir}/bin/user_default\")."
    action :create
    owner who
    user who
  end

  cookbook_file "#{homedir}/bin/user_default.erl" do
    path "#{homedir}/bin/user_default.erl"
    action :create_if_missing
    source "user_default.erl"
    owner who
    user who
  end

  execute "#{homedir}: set up user_default for erlang console use" do
    command "/opt/opscode/embedded/bin/erlc user_default.erl"
    cwd "#{homedir}/bin"
  end

  cookbook_file "#{homedir}: b2f" do
    path "#{homedir}/bin/b2f"
    mode "0777"
    action :create
    source "b2f"
    owner who
    user who
  end
end

file "/root/.bashrc" do
  content <<-EOC
  export PATH="/opt/opscode/bin:/opt/opscode/embedded/bin:$PATH"
  export EDITOR="vim"
  EOC
end

directory "/vagrant/testdata/cover" do
  action :create
  recursive true
end
directory "/vagrant/testdata/keys" do
  action :create
  recursive true
end
directory "/vagrant/testdata/orgs" do
  action :create
  recursive true
end

ruby_block "dotfiles" do
  block do
    require "fileutils"
    copyfiles = Dir.glob("/dotfiles/.*").reject { |name| name =~ /.*\.$/ }
    FileUtils.cp(copyfiles, "/home/vagrant")
  end
end

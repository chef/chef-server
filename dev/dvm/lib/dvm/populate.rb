require "erubis" # available courtesy of chef server.
require "fileutils"

# Note: we might want to use the API for all this, but for now it's
# just a glorified shell script
module DVM
  class Populator
    def initialize(orgs, users)
      @orgs = orgs
      @users = users
    end
    def make_orgs(orgs)
      FileUtils::mkdir_p '/vagrant/testdata/orgs'
      orgs.each do |name, data|
        @current_org = name
        make_org(data)
      end

    end
    def make_users(users)
      FileUtils::mkdir_p '/vagrant/testdata/keys'
      users.each do |user|
        make_user(user)
      end
    end
    def make_org(org)
      dir = "/vagrant/testdata/orgs/#{name}"
      FileUtils::mkdir_p dir
      validator = "#{dir}/#{name}-validator.pem"
      if File.exists? validator
        puts "Validator for #{name} exists, skipping org creation"
      else
        `#{Chef::Dist::Server::CTL} org-create #{name} #{name} > #{validator}`
      end
      org.users.each do |user|
        make_user(user)
        add_user_to_org(user, @current_org, false)
      end
      org.admins.each do |admin|
        make_user(user)
        add_user_to_org(user, @current_org, true)
      end
    end
    def add_user_to_org(username, orgname, admin)
      userdir = "/vagrant/testsdata/orgs/#{orgname}/#{username}/.chef"
      if File.exists? userdir
        puts "Not adding user #{username}, it is already in #{orgname}"
        return
      end
      File.mkdir_p userdir
      `#{Chef::Dist::Server::CTL} org-user-add #{orgname} #{username} -a #{admin}`

    end
    def make_user(user)
      key = user_key_path(user)
      if file.exists? key
        puts "Skipping user #{user}, key already exists"
      else
        # TODO we probably should care if this fails...
        `#{Chef::Dist::Server::CTL} user-create #{user} #{user} test-user #{username}@dvm.com password > #{key}`
      end
    end
    def user_key_path(username)
      "/vagrant/testdata/keys/#{username}.pem"
    end
    def validator_key_path
      "/vagrant/testdata/orgs/#{@current_org}/#{@current_org}-validtor.pemkeys/#{username}.pem"
    end
    def client_key_path(orgname, nodename)
      "/vagrant/testdata/orgs/#{orgname}/nodes/#{nodename}/.chef/#{nodename.pem}"
    end
    def kniferb(orgname, username, validatorpath, keypath)
      <<-EOM
log_level                :info
log_location             STDOUT
node_name                "#{username}"
client_key               "#{keypath}"
validation_client_name   "#{@current_org}-validator"
validation_key           "#{validatorpath}"
chef_server_url          "https://api.chef-server.dev/organizations/#{orgname}"
cache_type               'BasicFile'
cache_options( :path => "/vagrant/testdata/cookbookcache" )
cookbook_path            ["/vagrant/testdata/cookbooks"]
ssl_verify_mode  :verify_none
      EOM
    end
    def clientrb(orgname, nodename)
      <<-EOM
log_level                :info
log_location     STDOUT
client_key       "#{client_key_path(orgname, nodename)}"
chef_server_url  "https://api.chef-server.dev/organizations/#{orgname}
validation_client_name "#{orgname}-validator"
node_name "#{nodename}"
ssl_verify_mode  :verify_none
EOM

    end
  end
end
# TODO create dot chef dir
    # TODO turn of strict host checking in all knife.rbs
    # TODO knife in path?
    # TODO pivotal or just use validator?
    #command "chef-server-ctl user-create #{username} #{username} test-user #{username}@#{orgname}.com password > #{private_key}"
    #command "knife client create #{nodename} -u #{orgname}-validator -k #{org_validator} -s https://api.chef-server.dev > #{node_key_path}"
    #command "knife node create #{nodename} -u #{nodename} -k #{node_key_path} -s https://api.chef-server.dev"
    # Populate .chef/chef.rb for nodes, .chef/knife.rb for users
    #dot_chef = "#{org_root}/#{nodename}/.chef"
    #private_key = "#{user_root}/#{nodename}.pem"
    #template "#{dot_chef}/knife.rb" do
      #source "knife.rb.erb"
      #variables(
        #:username => username,
        #:orgname => orgname,
        #:server_fqdn => 'api.chef-server.dev'
      #)
      #mode "0777"
      #action :create
    #end
    # knife_command = "/opt/opscode/embedded/knife -s api.chef-server.dev -k /etc/opscode/pivotal.pem"


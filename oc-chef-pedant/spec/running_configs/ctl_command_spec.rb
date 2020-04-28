require 'json'
require 'pedant/rspec/common'

describe "running configs required by #{Chef::Dist::Server::CTL}", :config do
  let (:complete_config) { JSON.parse(IO.read("/etc/opscode/chef-server-running.json")) }
  let (:config) { complete_config['private_chef'] }

  context "partybus upgrade framework" do
    it "postgresql/vip" do
      expect(config["postgresql"]["vip"].to_s).to_not eq('')
    end

    it "postgresql/port" do
      expect(config["postgresql"]["port"].to_i).to_not eq(0)
    end

    it "postgresql/db_superuser" do
      expect(config["postgresql"]["db_superuser"].to_s).to_not eq('')
    end
  end

  context "migration 20" do
    it "opscode-erchef/sql_user" do
      expect(config["opscode-erchef"]["sql_user"].to_s).to_not eq('')
    end

    it "postgresql/vip" do
      expect(config["postgresql"]["vip"].to_s).to_not eq('')
    end

    it "postgresql/port" do
      expect(config["postgresql"]["port"].to_i).to_not eq(0)
    end
  end

  context "migration 31" do
    it "rabbitmq/user" do
      expect(config["rabbitmq"]["user"].to_s).to_not eq('')
    end

    it "rabbitmq/actions_user" do
      expect(config["rabbitmq"]["actions_user"].to_s).to_not eq('')
    end

    it "rabbitmq/management_user" do
      expect(config["rabbitmq"]["management_user"].to_s).to_not eq('')
    end
  end

  context "password" do
    it "ldap/enabled" do
      expect(config["ldap"]["enabled"]).to be(true).or be(false).or be(nil)
    end
  end

  context "runit" do
    it "runit/sv_dir" do
      expect(complete_config["runit"]["sv_dir"].to_s).to_not eq("")
      expect(File.exist?(complete_config["runit"]["sv_dir"])).to be(true)
    end
  end

  context "reindex" do
    it "fips_enabled" do
      expect(config['fips_enabled']).to be(true).or be(false)
    end

    it "opscode-erchef/search_queue_mode" do
      expect(config["opscode-erchef"]["search_queue_mode"]).to eq("rabbitmq")
                                                                 .or eq("batch")
                                                                       .or eq("inline")
    end

    it "redis_lb/vip" do
      expect(config["redis_lb"]["vip"].to_s).to_not eq("")
    end

    it "redis_lb/port" do
      expect(config["redis_lb"]["vip"].to_i).to_not eq(0)
    end
  end

  context "reconfigure" do
    #
    # The cookbooks themselves require chef-server-running.json to
    # populate previous_run.
    #
    it "bookshelf/storage_type" do
      expect(config["bookshelf"]["storage_type"]).to eq("filesystem").or eq("sql")
    end

    it "opscode-solr4/external" do
      expect(config["opscode-solr4"]["external"]).to eq(true).or eq(false)
    end

    it "postgresql/external" do
      expect(config["postgresql"]["external"]).to eq(true).or eq(false)
    end

    it "postgresql/data_dir" do
      expect(config["postgresql"]["data_dir"].to_s).to_not eq("")

      if config["postgresql"]["external"]
        skip "not used for external postgresql"
      # These tests should not run on the frontend of a tier setup.
      elsif (Pedant::Config.topology == "tier" && Pedant::Config.role == "frontend")
        skip "no postgresql installed on front-end of a tier server"
      else
        expect(File.exist?(config["postgresql"]["data_dir"])).to eq(true)
      end
    end
  end
end

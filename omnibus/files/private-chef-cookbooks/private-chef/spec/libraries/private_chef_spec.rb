require_relative '../../libraries/private_chef.rb'
require 'chef/log'

def expect_existing_secrets
  allow(File).to receive(:exists?).and_call_original
  allow(File).to receive(:exists?).with("/etc/opscode/private-chef-secrets.json").and_return(true)
  allow(File).to receive(:size).with("/etc/opscode/private-chef-secrets.json").and_return(1)
  allow(File).to receive(:exists?).with("/etc/opscode/pivotal.pem").and_return(false)
  allow(IO).to receive(:read).and_call_original
  allow(IO).to receive(:read).with("/etc/opscode/private-chef-secrets.json").and_return(secrets)
end

def config_for(hostname)
  PrivateChef.from_file(config_file)
  PrivateChef.generate_config(hostname)
end

describe PrivateChef do
  let(:node) {
    {
      "private_chef" => {
        "postgresql" => {
          "version" => "9.2"
        },
        "user" => { "username" => "opscode" },
      },
    }
  }

  before(:each) {
    PrivateChef.reset
    # May Cthulhu have mercy on our souls. PrivateChef.reset seems to do
    # too much without reloading the class, PrivateChef loses all of
    # the default Mashes
    Object.send(:remove_const, :PrivateChef)
    load ::File.expand_path("#{::File.dirname(__FILE__)}/../../libraries/private_chef.rb")
    allow(PrivateChef).to receive(:node).and_return(node)
    allow(PrivateChef).to receive(:exit!).and_raise(SystemExit)
    allow_any_instance_of(Veil::CredentialCollection::ChefSecretsFile).to receive(:save).and_return(true)
    allow(File).to receive(:exists?).with("/etc/opscode/private-chef-secrets.json").and_return(false)
  }

  # Example content of /etc/opscode/private-chef-secrets.json
  # used when testing non-bootstrap config parsing.
  let(:default_secrets) {<<EOF
  "redis_lb": {
    "password": "a24799bbeecee698792c6c9a26b453700bd52b709868a61f184cd9a0fdb32619cdeb494ddc98ce814aa14eda01fcc06cb335"
  },
  "rabbitmq": {
    "password": "a866861140c2c7bc2dc67c9f7696be2b2108321e18acb08922c28a075a8dbb8e773d82142e9cc52c96fdf6928c901c3ab360",
    "actions_password": "80ee6755aa6b4051aa99837ae213668f67f8941b6bb06142e0d9c99d9a4cd4210a07d30e430b49b41903b76554e01be11401",
    "management_password":"82ee6755aa6b4051aa99837ae213668f68f8941b6bb06142e0d9c99d9a4cd4210a07d30e430b49b41903b76554e01be11401"
  },
  "postgresql": {
    "db_superuser_password": "43e6fc68d8888764a7bea802a12be66b542f384b24469c6fc11a5d9c7c833b22962d8dd636ed2c5826a0aaeb056d49180391"
  },
  "opscode_erchef": {
    "sql_password": "d1f5ed48a14db08225d81fc8b8c50958318526ba86a3276effa0f24abf5b",
    "sql_ro_password": "96804edcfcf237db9925c734316faa3deb4cc3bc4e4e3a0f5e0be5bf3779"
  },
  "oc_id": {
    "sql_password": "8506ed8b4d4840dbd00da13157f48d4a362aac2101c3a1f4463e39e33ec46c7144681d1232cea80b7e7c382cc3f34b580f78",
    "sql_ro_password": "ca000f92407cca27995f925a5004aae08310819f3cd27dcb8cbd08e500b35f61acb2d98b709d39308b704d4481b2ee19b493",
    "secret_key_base": "df2c22dea04fd9b075c877a044499734ad6e6e9045ebf86f63bd27edbd7394bb3663f5a6af319a48ec4b70295164bd4811b2"
  },
  "drbd": {
    "shared_secret": "f872f5deb67b1a7faa9b47b829fb278c7ef83dc861e039ebe07255ec8618"
  },
  "oc_bifrost": {
    "superuser_id": "b293108b4a58a346758c091a8ca7462a",
    "sql_password": "dfaac052d330cb64aa82dd493bd342e93583cb61823fb3ed7792f21f928f9ef0b85d88c995b02ce216d193e9138217f8a5ed",
    "sql_ro_password": "f3003275a303dccc3e8b93bcde838e2254a82e1c6feb0db034e5e64c263e55643e14f8f75003c7080f7a145328d8a26c8242"
  },
  "bookshelf": {
    "sql_password": "8506ed8b4d4840dbd00da13157f48d4a362aac2101c3a1f4463e39e33ec46c7144681d1232cea80b7e7c382cc3f34b580f78",
    "sql_ro_password": "ca000f92407cca27995f925a5004aae08310819f3cd27dcb8cbd08e500b35f61acb2d98b709d39308b704d4481b2ee19b493",
    "access_key_id": "331fe88b6a86c4801218fd3e831a68b710544069",
    "secret_access_key": "393dd9330f834102f3650a6ac6938530ccbfbe1c86cb7d732f9893768e4e06eb172cc326da17f435"
  }
EOF
  }

  let(:secrets) { "{" + default_secrets + "}" }

  let(:config_file) {
    filename = "/fake/config.rb"
    allow(IO).to receive(:read).with(filename).and_return(config)
    filename
  }

  context "When ldap is enabled" do
    let(:config) { <<-EOF
  ldap["base_dn"] = "foo"
  ldap["host"] = "myhost"
  EOF
    }
    it "sets ldap['enabled'] to true" do
      rendered_config = config_for("api.chef.io")
      expect(rendered_config["private_chef"]["ldap"]["enabled"]).to eq(true)
    end
  end

  context "when ldap is NOT configured but bind_password is still in the secrets" do
    let(:secrets) {<<-EOF
      {
          #{default_secrets},
          "ldap": {
            "bind_password": "foobar"
          }
      }
      EOF
    }
    let(:config) { <<-EOF
  EOF
    }
    it "sets ldap['enabled'] to false" do
      expect_existing_secrets
      rendered_config = config_for("api.chef.io")
      expect(rendered_config["private_chef"]["ldap"]["bind_password"]).to eq("foobar")
      expect(rendered_config["private_chef"]["ldap"]["enabled"]).to eq(false)
    end
  end

  context "When FIPS is enabled at the kernel" do
    let(:config) { <<-EOF
fips true
EOF
    }
    it "sets fips_enabled to true" do
      rendered_config = config_for("api.chef.io")
      expect(rendered_config["private_chef"]["fips_enabled"]).to eq(true)
    end
  end

  context "when given types that need to be converted" do
    let(:config) { <<-EOF
bookshelf['storage_type'] = :filesystem
EOF
    }

    it "coverts bookshelf storage_type to a string" do
      rendered_config = config_for("api.chef.io")
      expect(rendered_config["private_chef"]["bookshelf"]["storage_type"]).to eq("filesystem")
    end
  end

  context "in a standalone topology" do
    let(:config) { <<-EOF
topology "standalone"
EOF
    }

    it "generates secrets" do
      rendered_config = config_for("api.chef.io")
      expect(PrivateChef.credentials.exist?("rabbitmq", "password")).to eq(true)
    end

    it "does not regenerate a secret if it already exists" do
      expect_existing_secrets
      config_for("api.chef.io")
      expect(PrivateChef.credentials.get("rabbitmq", "password")).to eq("a866861140c2c7bc2dc67c9f7696be2b2108321e18acb08922c28a075a8dbb8e773d82142e9cc52c96fdf6928c901c3ab360")
    end
  end

# HA is deprecated
  context "in an HA topology" do
    let(:config) { <<-EOF
topology "ha"

backend_vip "backend.chef.io",
  :ipaddress => "10.0.0.1"

server "frontend.chef.io",
  :role => 'frontend'

server "backend-active.chef.io",
  :role => 'backend',
  :bootstrap => true

server "backend-passive.chef.io",
  :role => 'backend'
EOF
    }

    before  do
      allow(File).to receive(:exists?).with("/etc/opscode/private-chef-secrets.json").and_return false
    end

    it "exits with a clear error warning that HA is unsupported" do
      expect(Chef::Log).to receive(:fatal).with(/DRBD_HA_002/)
      expect { config_for("backend-active.chef.io") }.to raise_error SystemExit
    end
    it "exits with a clear error warning that HA is unsupported" do
      expect(Chef::Log).to receive(:fatal).with(/DRBD_HA_002/)
      expect { config_for("backend-passive.chef.io") }.to raise_error SystemExit
    end
    it "exits with a clear error warning that HA is unsupported" do
      expect(Chef::Log).to receive(:fatal).with(/DRBD_HA_002/)
      expect { config_for("frontend.chef.io") }.to raise_error SystemExit
    end
  end

# Tiered
  context "in an tiered topology" do
    let(:config) { <<-EOF
topology "tier"

server "frontend.chef.io",
  :role => 'frontend'

backend_vip "backend.chef.io",
  :ipaddress => "10.0.0.1"

server "backend.chef.io",
  :role => 'backend',
  :bootstrap => true
EOF
    }

    before  do
      allow(File).to receive(:exists?).with("/etc/opscode/private-chef-secrets.json").and_return false
    end


    it "generates secrets on the bootstrap node" do
      config_for("backend.chef.io")
      expect(PrivateChef.credentials.exist?("rabbitmq", "password")).to eq(true)
    end

    it "enables opscode-chef-mover on the bootstrap node" do
      rendered_config = config_for("backend.chef.io")
      expect(rendered_config["private_chef"]["opscode-chef-mover"]["enable"]).to eq(true)
    end

    it "enables bootstrap recipe on the bootstrap node" do
      rendered_config = config_for("backend.chef.io")
      expect(rendered_config["private_chef"]["bootstrap"]["enable"]).to eq(true)
    end

    it "disables opscode-chef-mover on the frontend nodes" do
      expect_existing_secrets
      rendered_config = config_for("frontend.chef.io")
      expect(rendered_config["private_chef"]["opscode-chef-mover"]["enable"]).to eq(false)
    end

    it "disables bootstrap recipe on the frontend node" do
      expect_existing_secrets
      rendered_config = config_for("frontend.chef.io")
      expect(rendered_config["private_chef"]["bootstrap"]["enable"]).to eq(false)
    end

    it "sets backend services to listen on INADDR_ANY if the machine is a backend" do
      rendered_config = config_for("backend.chef.io")
      expect(rendered_config["private_chef"]["rabbitmq"]["node_ip_address"]).to eq("0.0.0.0")
      expect(rendered_config["private_chef"]["bookshelf"]["listen"]).to eq("0.0.0.0")
      expect(rendered_config["private_chef"]["redis_lb"]["listen"]).to eq("0.0.0.0")
      expect(rendered_config["private_chef"]["opscode-solr4"]["ip_address"]).to eq("0.0.0.0")
      expect(rendered_config["private_chef"]["postgresql"]["listen_address"]).to eq("*")
    end

    it "sets the VIPs for backend services to the backend_vip when configuring frontend services" do
      expect_existing_secrets
      rendered_config = config_for("frontend.chef.io")
      expect(rendered_config["private_chef"]["postgresql"]["vip"]).to eq("10.0.0.1")
      expect(rendered_config["private_chef"]["bookshelf"]["vip"]).to eq("10.0.0.1")
      expect(rendered_config["private_chef"]["redis_lb"]["vip"]).to eq("10.0.0.1")
      expect(rendered_config["private_chef"]["rabbitmq"]["vip"]).to eq("10.0.0.1")
      expect(rendered_config["private_chef"]["opscode-solr4"]["vip"]).to eq("10.0.0.1")
    end

    it "disables backend services on the frontend" do
      expect_existing_secrets
      rendered_config = config_for("frontend.chef.io")
      expect(rendered_config["private_chef"]["postgresql"]["enable"]).to eq(false)
      expect(rendered_config["private_chef"]["bookshelf"]["enable"]).to eq(false)
      expect(rendered_config["private_chef"]["redis_lb"]["enable"]).to eq(false)
      expect(rendered_config["private_chef"]["rabbitmq"]["enable"]).to eq(false)
      expect(rendered_config["private_chef"]["opscode-solr4"]["enable"]).to eq(false)
    end
  end

  describe "#generate_config" do
    context "when the topology is tiered" do
      let(:config) { <<-EOF
topology "tier"

server "frontend.chef.io",
  :role => "frontend"

server "backend.chef.io",
  :role => "backend",
  :bootstrap => true
EOF
      }

      it "exits with a clear error message if it can't find a server block for the current block for the current machine" do
        expect(Chef::Log).to receive(:fatal).with <<-EOF
No server configuration found for "backend-passive.chef.io" in /etc/opscode/chef-server.rb.
Server configuration exists for the following hostnames:

  backend.chef.io
  frontend.chef.io

EOF
        expect { config_for("backend-passive.chef.io") }.to raise_error SystemExit
      end
    end

    context "when a configuration extension is in use" do
      let(:config) {<<-EOF
topology "custom_topo"

server "a_backend",
  :role => "backend",
  :bootstrap => true

server "a_frontend",
  :role => "frontend"
EOF
      }

      let!(:my_gen_backend) {
        Proc.new {
          throw :my_gen_backend_called
        }
      }

      let!(:my_gen_frontend) {
        Proc.new {
          throw :my_gen_frontend_called
        }
      }

      let!(:my_gen_secrets) {
        Proc.new {
          throw :my_gen_secrets_called
        }
      }

      let!(:my_gen_api_fqdn) {
        Proc.new {
          throw :my_gen_api_fqdn_called
        }
      }

      let(:my_gen_api_fqdn_no_throw) { Proc.new {} }

      it "calls the gen_backup callback of the custom topology for a backend node" do
        PrivateChef.register_extension("custom_topo", gen_backend: my_gen_backend, server_config_required: true)
        expect{ config_for("a_backend") }.to throw_symbol :my_gen_backend_called
      end

      it "calls the gen_frontend callback of the custom topology for a frontend node" do
        PrivateChef.register_extension("custom_topo", gen_frontend: my_gen_frontend, server_config_required: true)
        expect{ config_for("a_frontend") }.to throw_symbol :my_gen_frontend_called
      end

      it "calls the gen_secrets callback of the custom topology on backend and frontend nodes" do
        PrivateChef.register_extension("custom_topo", gen_secrets: my_gen_secrets )
        expect{ config_for("a_frontend") }.to throw_symbol :my_gen_secrets_called
        expect{ config_for("a_backend") }.to throw_symbol :my_gen_secrets_called
      end

      it "calls the gen_api_fqdn callback of the custom topology on backend and frontend nodes" do
        PrivateChef.register_extension("custom_topo", gen_api_fqdn: my_gen_api_fqdn)
        expect{ config_for("a_frontend") }.to throw_symbol :my_gen_api_fqdn_called
        expect{ config_for("a_backend") }.to throw_symbol :my_gen_api_fqdn_called
      end

      it "uses the default implementation for a callback that isn't provided" do
        PrivateChef.register_extension("custom_topo", gen_api_fqdn: my_gen_api_fqdn_no_throw, server_config_required: true)
        rendered_config = config_for("a_backend")
        # test a known side-effect of teh current gen_backend function
        expect(rendered_config["private_chef"]["redis_lb"]["listen"]).to eq("0.0.0.0")
      end
    end
  end

  describe "#register_extension" do
    let(:name) { "mytopo" }
    let(:config) {<<-EOF
EOF
    }

    it "allows users to register new config values with a default" do
      PrivateChef.register_extension(name, config_values: {
                                       new_value: "a default" })
      expect(PrivateChef[:new_value]).to eq("a default")
    end

    it "renders registered configuration values into final config" do
      PrivateChef.register_extension(name, config_values: {
                                       new_value: "a default" })
      expect(config_for("some host")["private_chef"]["new-value"]).to eq("a default")
    end

    it "allows users to register callbacks for gen_backend" do
      my_gen_backend = lambda {
        "lamb"
      }

      PrivateChef.register_extension(name, gen_backend: my_gen_backend)
      expect(PrivateChef.registered_extensions[name][:gen_backend]).to eq(my_gen_backend)
    end

    it "allows users to register callbacks for gen_frontend" do
      my_gen_frontend = lambda {
        "lamb"
      }

      PrivateChef.register_extension(name, gen_frontend: my_gen_frontend)
      expect(PrivateChef.registered_extensions[name][:gen_frontend]).to eq(my_gen_frontend)
    end

    it "allows users to register callbacks for gen_secrets" do
      my_gen_secrets = lambda {
        "lamb"
      }

      PrivateChef.register_extension(name, gen_secrets: my_gen_secrets)
      expect(PrivateChef.registered_extensions[name][:gen_secrets]).to eq(my_gen_secrets)
    end


    it "allows users to register callbacks for gen_api_fqdn" do
      my_gen_api_fqdn = lambda {
        "lamb"
      }

      PrivateChef.register_extension(name, gen_api_fqdn: my_gen_api_fqdn)
      expect(PrivateChef.registered_extensions[name][:gen_api_fqdn]).to eq(my_gen_api_fqdn)
    end

    it "warns if you try to register a configuration value that already exists" do
      expect(Chef::Log).to receive(:warn).with("Extension different attempted to register configuration default for new_value, but it already exists!")
      PrivateChef.register_extension(name, config_values: {
                                       new_value: "a default" })
      PrivateChef.register_extension("different", config_values: {
                                       new_value: "a different default" })
    end

    it "warns if the extension contains un unkown configuration option" do
      expect(Chef::Log).to receive(:warn).with("Extension #{name} contains unknown configuration option: not_here")
      PrivateChef.register_extension(name, not_here: "a default")
    end

    it "does not save any unkown keys" do
      allow(Chef::Log).to receive(:warn)
      PrivateChef.register_extension(name, config_values: {
                                       not_here: "a default" })
      expect(PrivateChef["registered_extensions"][name][:not_here]).to eq(nil)
    end

  end
  context "Key File Migration" do
    let(:secrets_mock) { double(Object) }
    let(:superuser_key_path) { "/etc/opscode/pivotal.pem" }
    let(:webui_key_path) { "/etc/opscode/webui_priv.pem" }

    describe "#migrate_keys" do
      it "should attempt to migrate known keys" do
        expect(PrivateChef).to receive(:add_key_from_file_if_present).with("chef-server", "superuser_key", superuser_key_path)
        expect(PrivateChef).to receive(:add_key_from_file_if_present).with("chef-server", "webui_key", webui_key_path)
        PrivateChef.migrate_keys
      end
    end

    describe "#add_key_from_file_if_present" do
      before do
        allow(PrivateChef).to receive(:credentials).and_return secrets_mock
      end

      it "should add a key that exists and return true" do
        expect(File).to receive(:readable?).with("/my_key").and_return true
        expect(secrets_mock).to receive(:add_from_file).with("/my_key", "group", "name")
        result = PrivateChef.add_key_from_file_if_present("group", "name", "/my_key")
        expect(result).to be true
      end

      it "should not add a key that does not and return false" do
        expect(File).to receive(:readable?).with("/my_key").and_return false
        result = PrivateChef.add_key_from_file_if_present( "group", "name", "/my_key")
        expect(result).to be false

      end

    end
  end
end

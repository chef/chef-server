require_relative '../../libraries/private_chef.rb'
require 'chef/log'

def expect_existing_secrets
  allow(File).to receive(:exists?).and_call_original
  allow(File).to receive(:exists?).with('/etc/opscode/private-chef-secrets.json').and_return(true)
  allow(File).to receive(:size).with('/etc/opscode/private-chef-secrets.json').and_return(1)
  allow(File).to receive(:exists?).with('/etc/opscode/pivotal.pem').and_return(false)
  allow(IO).to receive(:read).and_call_original
  allow(IO).to receive(:read).with('/etc/opscode/private-chef-secrets.json').and_return(secrets)
end

def config_for(hostname)
  PrivateChef.from_file(config_file)
  PrivateChef.generate_config(hostname)
end

describe PrivateChef do
  let(:node) do
    {
      'private_chef' => {
        'postgresql' => {
          'version' => '9.2',
        },
        'user' => { 'username' => 'opscode' },
      },
    }
  end

  before(:each) do
    PrivateChef.reset
    # May Cthulhu have mercy on our souls. PrivateChef.reset seems to do
    # too much without reloading the class, PrivateChef loses all of
    # the default Mashes
    Object.send(:remove_const, :PrivateChef)
    load ::File.expand_path("#{::File.dirname(__FILE__)}/../../libraries/private_chef.rb")
    allow(PrivateChef).to receive(:node).and_return(node)
    allow(PrivateChef).to receive(:exit!).and_raise(SystemExit)
    allow_any_instance_of(Veil::CredentialCollection::ChefSecretsFile).to receive(:save).and_return(true)
    allow(File).to receive(:exists?).with('/etc/opscode/private-chef-secrets.json').and_return(false)
  end

  # Example content of /etc/opscode/private-chef-secrets.json
  # used when testing non-bootstrap config parsing.
  let(:default_secrets) do
    <<EOF
  "redis_lb": {
    "password": "a24799bbeecee698792c6c9a26b453700bd52b709868a61f184cd9a0fdb32619cdeb494ddc98ce814aa14eda01fcc06cb335"
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
  end

  let(:secrets) { '{' + default_secrets + '}' }

  let(:config_file) do
    filename = '/fake/config.rb'
    allow(IO).to receive(:read).with(filename).and_return(config)
    filename
  end

  context 'When ldap is enabled' do
    let(:config) do
      <<-EOF
  ldap["base_dn"] = "foo"
  ldap["host"] = "myhost"
  EOF
    end
    it "sets ldap['enabled'] to true" do
      rendered_config = config_for('api.chef.io')
      expect(rendered_config['private_chef']['ldap']['enabled']).to eq(true)
    end
  end

  context 'when ldap is NOT configured but bind_password is still in the secrets' do
    let(:secrets) do
      <<-EOF
      {
          #{default_secrets},
          "ldap": {
            "bind_password": "foobar"
          }
      }
      EOF
    end
    let(:config) do
      <<-EOF
  EOF
    end
    it "sets ldap['enabled'] to false" do
      expect_existing_secrets
      rendered_config = config_for('api.chef.io')
      expect(rendered_config['private_chef']['ldap']['bind_password']).to eq('foobar')
      expect(rendered_config['private_chef']['ldap']['enabled']).to eq(false)
    end
  end

  context 'When FIPS is enabled at the kernel' do
    let(:config) do
      <<~EOF
        fips true
      EOF
    end
    it 'sets fips_enabled to true' do
      rendered_config = config_for('api.chef.io')
      expect(rendered_config['private_chef']['fips_enabled']).to eq(true)
    end
  end

  context 'when given types that need to be converted' do
    let(:config) do
      <<~EOF
        bookshelf['storage_type'] = :filesystem
      EOF
    end

    it 'coverts bookshelf storage_type to a string' do
      rendered_config = config_for('api.chef.io')
      expect(rendered_config['private_chef']['bookshelf']['storage_type']).to eq('filesystem')
    end
  end

  context 'in a standalone topology' do
    let(:config) do
      <<~EOF
        topology "standalone"
      EOF
    end

    it 'generates secrets' do
      rendered_config = config_for('api.chef.io')
      expect(PrivateChef.credentials.exist?('redis_lb', 'password')).to eq(true)
    end

    it 'does not regenerate a secret if it already exists' do
      expect_existing_secrets
      config_for('api.chef.io')
      expect(PrivateChef.credentials.get('redis_lb', 'password')).to eq('a24799bbeecee698792c6c9a26b453700bd52b709868a61f184cd9a0fdb32619cdeb494ddc98ce814aa14eda01fcc06cb335')
    end
  end

  # HA is deprecated
  context 'in an HA topology' do
    let(:config) do
      <<~EOF
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
    end

    before do
      allow(File).to receive(:exists?).with('/etc/opscode/private-chef-secrets.json').and_return false
    end

    it 'exits with a clear error warning that HA is unsupported' do
      expect(Chef::Log).to receive(:fatal).with(/DRBD_HA_002/)
      expect { config_for('backend-active.chef.io') }.to raise_error SystemExit
    end
    it 'exits with a clear error warning that HA is unsupported' do
      expect(Chef::Log).to receive(:fatal).with(/DRBD_HA_002/)
      expect { config_for('backend-passive.chef.io') }.to raise_error SystemExit
    end
    it 'exits with a clear error warning that HA is unsupported' do
      expect(Chef::Log).to receive(:fatal).with(/DRBD_HA_002/)
      expect { config_for('frontend.chef.io') }.to raise_error SystemExit
    end
  end

  # Tiered
  context 'in an tiered topology' do
    let(:config) do
      <<~EOF
        topology "tier"

        server "frontend.chef.io",
          :role => 'frontend'

        backend_vip "backend.chef.io",
          :ipaddress => "10.0.0.1"

        server "backend.chef.io",
          :role => 'backend',
          :bootstrap => true
      EOF
    end

    before do
      allow(File).to receive(:exists?).with('/etc/opscode/private-chef-secrets.json').and_return false
    end

    it 'generates secrets on the bootstrap node' do
      config_for('backend.chef.io')
      expect(PrivateChef.credentials.exist?('redis_lb', 'password')).to eq(true)
    end

    it 'enables opscode-chef-mover on the bootstrap node' do
      rendered_config = config_for('backend.chef.io')
      expect(rendered_config['private_chef']['opscode-chef-mover']['enable']).to eq(true)
    end

    it 'enables bootstrap recipe on the bootstrap node' do
      rendered_config = config_for('backend.chef.io')
      expect(rendered_config['private_chef']['bootstrap']['enable']).to eq(true)
    end

    it 'disables opscode-chef-mover on the frontend nodes' do
      expect_existing_secrets
      rendered_config = config_for('frontend.chef.io')
      expect(rendered_config['private_chef']['opscode-chef-mover']['enable']).to eq(false)
    end

    it 'disables bootstrap recipe on the frontend node' do
      expect_existing_secrets
      rendered_config = config_for('frontend.chef.io')
      expect(rendered_config['private_chef']['bootstrap']['enable']).to eq(false)
    end

    it 'sets backend services to listen on INADDR_ANY if the machine is a backend' do
      rendered_config = config_for('backend.chef.io')
      expect(rendered_config['private_chef']['bookshelf']['listen']).to eq('0.0.0.0')
      expect(rendered_config['private_chef']['redis_lb']['listen']).to eq('0.0.0.0')
      expect(rendered_config['private_chef']['postgresql']['listen_address']).to eq('*')
    end

    it 'sets the VIPs for backend services to the backend_vip when configuring frontend services' do
      expect_existing_secrets
      rendered_config = config_for('frontend.chef.io')
      expect(rendered_config['private_chef']['postgresql']['vip']).to eq('10.0.0.1')
      expect(rendered_config['private_chef']['bookshelf']['vip']).to eq('10.0.0.1')
      expect(rendered_config['private_chef']['redis_lb']['vip']).to eq('10.0.0.1')
    end

    it 'disables backend services on the frontend' do
      expect_existing_secrets
      rendered_config = config_for('frontend.chef.io')
      expect(rendered_config['private_chef']['postgresql']['enable']).to eq(false)
      expect(rendered_config['private_chef']['bookshelf']['enable']).to eq(false)
      expect(rendered_config['private_chef']['redis_lb']['enable']).to eq(false)
    end
  end

  context 'When we disable _status reporting for the data_collector endpoint' do
    let(:config) { "data_collector['health_check'] = false" }
    it "sets ['private_chef']['data_collector']['health_check'] to false" do
      rendered_config = config_for('api.chef.io')
      expect(rendered_config['private_chef']['data_collector']['health_check']).to be_falsey
    end
  end

  context 'When we enable _status reporting for the data_collector endpoint' do
    context 'using a boolean' do
      let(:config) { "data_collector['health_check'] = true" }
      it "sets ['private_chef']['data_collector']['health_check'] truthy" do
        rendered_config = config_for('api.chef.io')
        expect(rendered_config['private_chef']['data_collector']['health_check']).to be_truthy
      end
    end
    context 'using a string' do
      let!(:config) { "data_collector['health_check'] = 'true'" }
      it "sets ['private_chef']['data_collector']['health_check'] truthy" do
        rendered_config = config_for('api.chef.io')
        expect(rendered_config['private_chef']['data_collector']['health_check']).to be_truthy
      end
    end
  end

  describe '#generate_config' do
    context 'when the topology is tiered' do
      let(:config) do
        <<~EOF
          topology "tier"

          server "frontend.chef.io",
            :role => "frontend"

          server "backend.chef.io",
            :role => "backend",
            :bootstrap => true
        EOF
      end

      it "exits with a clear error message if it can't find a server block for the current block for the current machine" do
        expect(Chef::Log).to receive(:fatal).with <<~EOF
          No server configuration found for "backend-passive.chef.io" in /etc/opscode/chef-server.rb.
          Server configuration exists for the following hostnames:

            backend.chef.io
            frontend.chef.io

        EOF
        expect { config_for('backend-passive.chef.io') }.to raise_error SystemExit
      end
    end
  end

  context 'Key File Migration' do
    let(:secrets_mock) { double(Object) }
    let(:superuser_key_path) { '/etc/opscode/pivotal.pem' }
    let(:webui_key_path) { '/etc/opscode/webui_priv.pem' }

    describe '#migrate_keys' do
      it 'should attempt to migrate known keys' do
        expect(PrivateChef).to receive(:add_key_from_file_if_present).with('chef-server', 'superuser_key', superuser_key_path)
        expect(PrivateChef).to receive(:add_key_from_file_if_present).with('chef-server', 'webui_key', webui_key_path)
        PrivateChef.migrate_keys
      end
    end

    describe '#add_key_from_file_if_present' do
      before do
        allow(PrivateChef).to receive(:credentials).and_return secrets_mock
      end

      it 'should add a key that exists and return true' do
        expect(File).to receive(:readable?).with('/my_key').and_return true
        expect(secrets_mock).to receive(:add_from_file).with('/my_key', 'group', 'name')
        result = PrivateChef.add_key_from_file_if_present('group', 'name', '/my_key')
        expect(result).to be true
      end

      it 'should not add a key that does not and return false' do
        expect(File).to receive(:readable?).with('/my_key').and_return false
        result = PrivateChef.add_key_from_file_if_present('group', 'name', '/my_key')
        expect(result).to be false
      end
    end
  end
end

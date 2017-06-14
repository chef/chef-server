require 'json'
require 'pedant/rspec/common'

describe 'running configs required by Analytics Server', :config do
  #
  # The analyics add-on reads from actions-source.json which is only
  # written out in some configurations.
  #
  running_config = JSON.parse(IO.read('/etc/opscode/chef-server-running.json'))
  if !(running_config['private_chef']['insecure_addon_compat'] && running_config['private_chef']['dark_launch']['actions'])
    skip 'Analytics config is only written when insecure_addon_compat = true && dark_launch["actions"] = true'
  else
    let(:actions_source) { JSON.parse(IO.read('/etc/opscode-analytics/actions-source.json')) }
    let(:config) { actions_source['private_chef'] }

    it 'api_fqdn' do
      expect(config['api_fqdn'].to_s).to_not eq('')
    end

    it 'oc_id_application' do
      expect(config['oc_id_application'].class).to eq(Hash)
    end

    it 'rabbitmq_host' do
      expect(config['rabbitmq_host'].to_s).to_not eq('')
    end

    it 'rabbitmq_port' do
      expect(config['rabbitmq_port'].to_i).to_not eq(0)
    end

    it 'rabbitmq_vhost' do
      expect(config['rabbitmq_vhost'].to_s).to_not eq('')
    end

    it 'rabbitmq_exchange' do
      expect(config['rabbitmq_exchange'].to_s).to_not eq('')
    end

    it 'rabbitmq_user' do
      expect(config['rabbitmq_user'].to_s).to_not eq('')
    end

    it 'rabbitmq_password' do
      expect(config['rabbitmq_password'].to_s).to_not eq('')
    end
  end
end

require 'spec_helper'

describe 'The web app' do

  describe command('which ruby') do
    its(:stdout) { should match /ruby/ }
  end

  describe command('which git') do
    its(:stdout) { should match /git/ }
  end

  describe command('which node') do
    its(:stdout) { should match /node/ }
  end

  describe user('opscode') do
    it { should exist }
  end

  describe group('opscode') do
    it { should exist }
  end

  describe file('/etc/opscode/webui_priv.pem') do
    it { should be_file }
  end

  describe file('/srv/oc-id') do
    it { should be_directory }
  end

  describe file('/srv/oc-id/current') do
    it { should be_directory }
  end

  describe file('/etc/opscode/webui_priv.pem') do
    it { should be_file }
    it { should be_readable.by_user('opscode') }
  end

  describe file('/srv/oc-id/current/config/settings/production.yml') do
    it { should be_file }
    it { should be_readable.by_user('opscode') }
  end

  describe file('/var/log/oc-id.log') do
    it { should be_file }
  end

  describe file('/etc/unicorn/oc-id.rb') do
    it { should be_file }
  end

  describe service('oc-id') do
    it { should be_running }
  end

  describe service('postgresql') do
    pending do
      it { should be_running }
    end
  end

  describe port(4060) do
    it { should be_listening }
  end

end

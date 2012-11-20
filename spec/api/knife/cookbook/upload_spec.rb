# -*- coding: utf-8 -*-
#
# Author:: Christopher Maier (<cm@opscode.com>)
# Author:: Ho-Sheng Hsiao (<hosh@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.

require 'pedant/rspec/knife_util'

describe 'Private Chef knife Tests', :knife do
  context 'cookbook' do
    context 'upload' do
      include Pedant::RSpec::KnifeUtil

      let(:command) { "knife cookbook upload #{cookbook_name} -c #{knife_config}" }
      let(:cookbook_name){ "joy_of_cooking" }

      before :all do
        knife_admin # just referring to the user to ensure it's created,
        # since we don't actually need to refer to it in the
        # test, as we do when exercising the REST API
      end

      after(:each) { knife "cookbook delete #{cookbook_name} -c #{knife_config} --yes" }

      context 'as a normal user' do
        let(:knife_config) { knife_config_for_normal_user }

        it 'should succeed', :slow => !open_source? do
          should have_outcome :status => 0, :stdout => /Uploaded 1 cookbook/
        end
      end

    end
  end
end

# -*- coding: utf-8 -*-
#
# Author:: Seth Falcon (<seth@chef.io>)
# Author:: James Casey (<james@chef.io>)
# Author:: Douglas Triggs (<doug@chef.io>)
# Author:: Ho-Sheng Hsiao (<hosh@chef.io>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'pp'
require 'pedant/rspec/cookbook_util'

describe "Cookbooks API endpoint", :cookbooks do
  let(:cookbook_url_base) { "cookbooks" }
  include Pedant::RSpec::CookbookUtil


  context "DELETE /cookbooks/<name>/<version>" do
    let(:request_method) { :DELETE }
    let(:request_url)    { named_cookbook_url }
    let(:requestor)      { admin_user }

    let(:original_cookbook) { new_cookbook(cookbook_name, cookbook_version) }
    let(:fetched_cookbook) { original_cookbook }

    context 'with delete cookbook authorization' do
      let(:cookbook_name) {"delete-cookbook"}
      let(:cookbook_version) { "0.0.1" }

      before(:each) { make_cookbook(admin_user, cookbook_name, cookbook_version) }
      after(:each)  { delete_cookbook(admin_user, cookbook_name, cookbook_version) }

      context 'as an authorized, normal user' do
        let(:expected_response) { delete_cookbook_success_response }
        let(:requestor) { normal_user }

        it "should respond with 200 (\"OK\")" do
          should look_like expected_response
          should_be_deleted
        end # it normal user returns 200
      end # as an authorized, normal user

      context 'as an unauthorized, normal user' do
        let(:expected_response) { forbidden_action_response }
        let(:requestor) { normal_user }
        let(:restrict_permissions!) do
          restrict_permissions_to "/cookbooks/#{cookbook_name}",
                                  normal_user => ['read'],
                                   admin_user => ['read', 'delete']
        end


        it "should respond with 403 (\"Forbidden\")", :authorization do
          restrict_permissions!

          should look_like expected_response
          should_not_be_deleted
        end # it user with no perms can't delete a cookbook
      end

    end
  end

end # describe Cookbooks API endpoint

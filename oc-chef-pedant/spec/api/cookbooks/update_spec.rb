# -*- coding: utf-8 -*-
# Copyright: Copyright (c) 2015 Chef Software, Inc.
# License: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require 'pedant/rspec/cookbook_util'

# FIXME For /cookbooks/NAME/VERSION tests we have a limited checking
# on the GET afterwards to do validation of data on the server.  Since
# we can't match the payload against one with dynamically generated
# URLs we're only checking for the return code.
#
# We need to come back (along with adding tests for the GET case
# explicitly in read_spec.rb) and update the tests marked with TODO
# to actually match on the generate response body as well
#

describe "Cookbooks API endpoint", :cookbooks, :cookbooks_update do

  include Pedant::RSpec::CookbookUtil

  context "PUT /cookbooks/<name>/<version> [update]" do

    let(:request_method){:PUT}
    shared(:requestor){admin_user}
    let(:request_url)    { api_url("/cookbooks/#{cookbook_name}/#{cookbook_version}") }
    let(:cookbook_name) { "cookbook-to-be-modified" }
    let(:cookbook_version) { self.class.cookbook_version }
    let(:fetched_cookbook) { retrieved_cookbook(cookbook_name, cookbook_version) }
    let(:original_cookbook) { new_cookbook(cookbook_name, cookbook_version) }

    # This requires deep dup
    let(:updated_cookbook) do
      original_cookbook.dup.tap do |cookbook|
        cookbook["metadata"] = cookbook["metadata"].dup.tap { |c| c["description"] = "hi there #{rand(10000)}" }
      end
    end

    # TODO: KLUDGE: Cop-out, because I am too tired to refactor the macros correctly
    def self.cookbook_version
      "11.2.3"
    end

    before(:each) {
      make_cookbook("/cookbooks/#{cookbook_name}/#{cookbook_version}")
    }

    respects_maximum_payload_size

    context "as admin user" do
      it "should respond with 200 Ok", :smoke do
        payload = new_cookbook(cookbook_name, cookbook_version)
        metadata = payload["metadata"]
        metadata["description"] = "hi there"
        payload["metadata"] = metadata

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}"), payload: payload)
        ).to look_like(status: 200, body_exact: payload)

        # verify change happened
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}"))
        ).to look_like(status: 200, body_exact: payload)
      end # it admin user returns 200

      context 'as a user outside of the organization', :authorization do
        it "should respond with 403 (\"Forbidden\") and does not update cookbook" do
          expect(
            put(request_url, outside_user, payload: updated_cookbook)
          ).to look_like(unauthorized_access_credential_response)
          should_not_be_updated
        end # it outside user returns 403 and does not update cookbook
      end

      context 'with invalid user', :authentication do
        it "returns 401 and does not update cookbook" do
          expect(
            put("/cookbooks/#{cookbook_name}/#{cookbook_version}", invalid_user, payload: updated_cookbook)
          ).to look_like(invalid_credential_exact_response)

          # Verified change did not happen
          expect(
            get("/cookbooks/#{cookbook_name}/#{cookbook_version}"))
          ).to look_like(status: 200, body_exact: original_cookbook)
        end # it invalid user returns 401 and does not update cookbook
      end # with invalid user
    end # context with permissions for

    context "for checksums" do
      include Pedant::RSpec::CookbookUtil

      let(:sandbox) { create_sandbox(files) }
      let(:upload) { ->(file) { upload_to_sandbox(file, sandbox) } }
      let(:files) { (0..3).to_a.map { Pedant::Utility.new_random_file } }

      let(:committed_files) do
        files.each(&upload)
        result = commit_sandbox(sandbox)
        result
      end

      let(:checksums) { parse(committed_files)["checksums"] }

      it "adding all new checksums should succeed" do
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["files"] = [{"name" => "name1", "path" => "files/default/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "files/default/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"},
                            {"name" => "name3", "path" => "files/default/name3",
                              "checksum" => checksums[2],
                              "specificity" => "default"},
                            {"name" => "name4", "path" => "files/default/name4",
                              "checksum" => checksums[3],
                              "specificity" => "default"}]

        verify_checksum_cleanup("/cookbooks/#{cookbook_name}/#{cookbook_version}") do

          expect(
            put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
          ).to look_like(status: 200, body_exact: payload)

          # verify change happened
          # TODO make this match on body when URLs are parsable
          expect(
            get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
          ).to look_like(status: 200)
        end # verify_checksum_cleanup

      end # it adding all new checksums should succeed

      it "should return url when adding checksums" do
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["files"] = [{"name" => "name1", "path" => "files/default/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "files/default/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"}]

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}"), payload: payload)
        ).to look_like(status: 200, body: payload)

        # TODO original description indicated ruby returned URI, and also b ody_exact was commented out below.
        # Look into it ...
        # verify change happened
        # TODO make this match on body when URLs are parsable
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200, body: payload)
      end

      it "adding invalid checksum should fail", :validation do
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["files"] = [{"name" => "name1", "path" => "files/path/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "files/path/name2",
                              "checksum" => "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                              "specificity" => "default"}]

        error = ["Manifest has checksum aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa but it hasn't yet been uploaded"]
        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}"), payload: payload
        ).to look_like(status: 400, body_exact: { "error" => error })

        # Verify change did not happen
        payload.delete("files")

        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200, body: payload)
      end # it adding invalid checksum should fail

      it "deleting all checksums should succeed" do
        expect(
          delete("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["files"] = [{"name" => "name1", "path" => "files/default/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "files/default/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"},
                            {"name" => "name3", "path" => "files/default/name3",
                              "checksum" => checksums[2],
                              "specificity" => "default"},
                            {"name" => "name4", "path" => "files/default/name4",
                              "checksum" => checksums[3],
                              "specificity" => "default"}]
        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
        ).to look_like(status: 201)

        # Verified initial cookbook
        # TODO make this match on body when URLs are parsable
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        verify_checksum_cleanup("/cookbooks/#{cookbook_name}/#{cookbook_version}") do

          payload.delete("files")
          expect(
            put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
          ).to look_like(status: 200, body_exact: payload)

          # verify change happened
          # TODO make this match on body when URLs are parsable
          expect(
            get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
          ).to look_like(status: 200)
        end # verify_checksum_cleanup

      end # it deleting all checksums should succeed

      it "deleting some checksums should succeed" do
        expect(
          delete("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["files"] = [{"name" => "name1", "path" => "path/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "path/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"},
                            {"name" => "name3", "path" => "path/name3",
                              "checksum" => checksums[2],
                              "specificity" => "default"},
                            {"name" => "name4", "path" => "path/name4",
                              "checksum" => checksums[3],
                              "specificity" => "default"}]

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
        ).to look_like(status: 201)

        # Verified initial cookbook
        # TODO make this match on body when URLs are parsable
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        verify_checksum_cleanup("/cookbooks/#{cookbook_name}/#{cookbook_version}") do

          payload["files"] = [{"name" => "name1", "path" => "path/name1",
                                "checksum" => checksums[0],
                                "specificity" => "default"},
                              {"name" => "name2", "path" => "path/name2",
                                "checksum" => checksums[1],
                                "specificity" => "default"}]
          expect(
            put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
          ).to look_like(status: 200, body_exact: payload)

          # verify change happened
          # TODO make this match on body when URLs are parsable
          expect(
            get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
          ).to look_like(status: 200)
        end # verify_checksum_cleanup
      end # it deleting some checksums should succeed

      it "changing all different checksums should succeed" do
        expect(
          delete("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["files"] = [{"name" => "name1", "path" => "path/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "path/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"}]
        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
        ).to look_like(status: 201)

        # Verified initial cookbook
        # TODO make this match on body when URLs are parsable
        response = get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        expect(response).to look_like(status: 200)

        verify_checksum_cleanup("/cookbooks/#{cookbook_name}/#{cookbook_version}") do

          payload["files"] = [{"name" => "name3", "path" => "path/name3",
                                "checksum" => checksums[2],
                                "specificity" => "default"},
                              {"name" => "name4", "path" => "path/name4",
                                "checksum" => checksums[3],
                                "specificity" => "default"}]
          response = put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
          expect(response).to look_like(status: 200, body_exact: payload)

          # verify change happened
          # TODO make this match on body when URLs are parsable
          response = get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
          expect(response).to look_like(status: 200)
        end # verify_checksum_cleanup
      end # it changing all different checksums should succeed

      it "changing some different checksums should succeed" do
        expect(
          delete("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["files"] = [{"name" => "name1", "path" => "path/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "path/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"},
                            {"name" => "name3", "path" => "path/name3",
                              "checksum" => checksums[2],
                              "specificity" => "default"}]

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
        ).to look_like(status: 201)

        # Verified initial cookbook
        # TODO make this match on body when URLs are parsable
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        verify_checksum_cleanup("/cookbooks/#{cookbook_name}/#{cookbook_version}") do

          payload["files"] = [{"name" => "name2", "path" => "path/name2",
                                "checksum" => checksums[1],
                                "specificity" => "default"},
                              {"name" => "name3", "path" => "path/name3",
                                "checksum" => checksums[2],
                                "specificity" => "default"},
                              {"name" => "name4", "path" => "path/name4",
                                "checksum" => checksums[3],
                                "specificity" => "default"}]
          expect(
            put("/cookbooks/#{cookbook_name}/#{cookbook_version}")
          ).to look_like(status: 200, body_exact: payload)

          # verify change happened
          # TODO make this match on body when URLs are parsable
          expect(
            get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
          ).to look_like(status: 200)
        end # verify_checksum_cleanup
      end # it changing some different checksums should succeed

      it "changing to invalid checksums should fail", :validation do
        expect(
          delete("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["files"] = [{"name" => "name1", "path" => "path/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "path/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"},
                            {"name" => "name3", "path" => "path/name3",
                              "checksum" => checksums[2],
                              "specificity" => "default"}]
        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
        ).to look_like(status: 201)

        # Verified initial cookbook
        # TODO make this match on body when URLs are parsable
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)

        payload["files"] = [{"name" => "name2", "path" => "path/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"},
                            {"name" => "name3", "path" => "path/name3",
                              "checksum" => checksums[2],
                              "specificity" => "default"},
                            {"name" => "name4", "path" => "path/name4",
                              "checksum" => "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                              "specificity" => "default"}]
        error = ["Manifest has checksum aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa but it hasn't yet been uploaded"]

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
        ).to look_like(status: 400, body_exact: { "error" => error }

        # verify change did not happen
        payload["files"] = [{"name" => "name1", "path" => "path/name1",
                              "checksum" => checksums[0],
                              "specificity" => "default"},
                            {"name" => "name2", "path" => "path/name2",
                              "checksum" => checksums[1],
                              "specificity" => "default"},
                            {"name" => "name3", "path" => "path/name3",
                              "checksum" => checksums[2],
                              "specificity" => "default"}]

        # TODO make this match on body when URLs are parsable
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200)
      end # it changing to invalid checksums should fail

      # Coverge for CHEF-3716
      #
      # The ultimate problem was that we were inadvertently deleting some files
      # from S3 / Bookshelf on cookbook updates. When a file was no longer
      # referenced by that cookbook version, we would delete it without first
      # checking that it wasn't being referenced by any other cookbooks. The
      # database was internally consistent (modulo some "garbage" checksums
      # remaining), but it was inconsistent with S3 / Bookshelf, which resulted
      # in the 404 errors when trying to download.
      context "CHEF-3716 coverage" do
        let(:cookbook_version2) { "11.2.4" }

        it "it does not delete checksums in use by another version" do

          # Might be a little paranoid, but lets ensure the version strings
          # versions are indeed different since the whole test hinges on this.
          cookbook_version.should_not eq cookbook_version2

          # Create two cookbook versions that share a single file
          payload1 = new_cookbook(cookbook_name, cookbook_version)
          payload1["files"] = [{"name" => "name1", "path" => "path/name1",
                                "checksum" => checksums[0],
                                "specificity" => "default"},
                              {"name" => "name2", "path" => "path/name2",
                                "checksum" => checksums[1],
                                "specificity" => "default"}]
          payload2 = new_cookbook(cookbook_name, cookbook_version2)
          payload2["files"] = [{"name" => "name1", "path" => "path/name1",
                                "checksum" => checksums[0],
                                "specificity" => "default"},
                              {"name" => "name2", "path" => "path/name2",
                                "checksum" => checksums[2],
                                "specificity" => "default"}]
          expect(
            put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload1)
          ).to look_like(status: 201)
          expect(
            put("/cookbooks/#{cookbook_name}/#{cookbook_version2}", payload: payload2)
          ).to look_like(status: 201)

          # compute an intersection and difference
          cbv_1_checksums = get_cookbook_checksums("/cookbooks/#{cookbook_name}/#{cookbook_version}")
          cbv_2_checksums = get_cookbook_checksums("/cookbooks/#{cookbook_name}/#{cookbook_version2}")
          intersection_checksums = cbv_1_checksums.keys & cbv_2_checksums.keys
          cbv_2_difference_checksums = cbv_2_checksums.keys - cbv_1_checksums.keys

          # Make changes to the files in cookbook version 2. This effectively
          # deletes all the old files.
          payload2["files"] = [{"name" => "name5", "path" => "path/name5",
                                "checksum" => checksums[3],
                                "specificity" => "default"}]
          expect(
            put("/cookbooks/#{cookbook_name}/#{cookbook_version2}", payload: payload2)
          ).to look_like(status: 201)

          # Checksums unique to first iteration of cookbook version 2 should
          # have been deleted
          cbv_2_difference_checksums.each do |checksum|
            verify_checksum_url(cbv_2_checksums[checksum], 404)
          end

          # Checksums shared between the original iterations of the cookbook
          # versions should still exist
          intersection_checksums.each do |checksum|
            verify_checksum_url(cbv_2_checksums[checksum], 200)
          end
        end
      end

    end # context for checksums

    context "for frozen?" do
      before(:each) do
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["frozen?"] = true

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
        ).to look_like(status: 200, body_exact: payload)
      end # before :each

      it "can set frozen? to true" do
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["frozen?"] = true
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200, body: payload)
      end # it can set frozen? to true

      it "can not edit cookbook when frozen? is set to true" do
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["frozen?"] = false
        metadata = payload["metadata"]
        metadata["description"] = "this is different"
        payload["metadata"] = metadata

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}", payload: payload)
        ).to look_like(status: 409, body_exact: {
          "error" => ["The cookbook #{cookbook_name} at version #{cookbook_version} is frozen. Use the 'force' option to override."]
        })

        # Verify that change did not occur
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["frozen?"] = true
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200, body: payload)
      end # it can not edit cookbook when frozen? is set to true

      it "can override frozen? with force set to true" do
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["frozen?"] = false
        metadata = payload["metadata"]
        metadata["description"] = "this is different"
        payload["metadata"] = metadata

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}?force=true", payload: payload)
        ).to look_like(status: 200, body_exact: payload)

        # Verify that change did occur
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200, body: payload)
      end # it can override frozen? with force set to true

      it "can not override frozen? with force set to false" do
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["frozen?"] = false
        metadata = payload["metadata"]
        metadata["description"] = "this is different"
        payload["metadata"] = metadata

        expect(
          put("/cookbooks/#{cookbook_name}/#{cookbook_version}?force=false", payload: payload)
        ).to look_like(status: 409, body_exact: {
          "error" => ["The cookbook #{cookbook_name} at version #{cookbook_version} is frozen. Use the 'force' option to override."]
        })

        # Verify that change did occur
        payload = new_cookbook(cookbook_name, cookbook_version)
        payload["frozen?"] = true
        expect(
          get("/cookbooks/#{cookbook_name}/#{cookbook_version}")
        ).to look_like(status: 200, body: payload)
      end # it can not override frozen? with force set to false
    end # context for frozen?

    context "when modifying data" do
      context "for cookbook_name" do
        [1, true, [], {}].each do |value|
          should_fail_to_change('cookbook_name', value, 400, "Field 'cookbook_name' invalid")
        end
        ['new_cookbook_name', 'with a space', '外国語'].each do |value|
          should_fail_to_change('cookbook_name', value, 400, "Field 'cookbook_name' invalid")
        end
        should_fail_to_change('cookbook_name', :delete, 400, "Field 'cookbook_name' missing")
      end # context for cookbook_name

      context "for json_class" do
        should_not_change('json_class', :delete, 'Chef::CookbookVersion')
        should_fail_to_change('json_class', 1, 400, "Field 'json_class' invalid")
        should_fail_to_change('json_class', 'Chef::NonCookbook', 400, "Field 'json_class' invalid")
        should_fail_to_change('json_class', 'all wrong', 400, "Field 'json_class' invalid")
      end # context for json_class

      context "for chef_type" do
        should_not_change('chef_type', :delete, 'cookbook_version')
        should_fail_to_change('chef_type', 'not_cookbook', 400, "Field 'chef_type' invalid")
        should_fail_to_change('chef_type', false, 400, "Field 'chef_type' invalid")
        should_fail_to_change('chef_type', ['just any', 'old junk'], 400, "Field 'chef_type' invalid")
      end # context for chef_type

      context "for version" do
        should_change('version', :delete)
        error = "Field 'version' invalid"
        should_fail_to_change('version', 1, 400, error)
        should_fail_to_change('version', ['all', 'ignored'], 400, error)
        should_fail_to_change('version', {}, 400, error)

        error = "Field 'version' invalid"
        should_fail_to_change('version', '0.0', 400, error)
        should_fail_to_change('version', 'something invalid', 400, error)
      end # context for version

      context "for collections" do
        ['attributes', 'definitions', 'files', 'libraries', 'providers', 'recipes',
         'resources', 'root_files', 'templates'].each do |segment|
          context "for #{segment}" do
            should_fail_to_change(segment, 'foo', 400, "Field '#{segment}' invalid")
            error = "Invalid element in array value of '#{segment}'."
            should_fail_to_change(segment, ['foo'], 400, error)
            should_change(segment, [])
            should_fail_to_change(segment, [{}, {}], 400, error)
            should_fail_to_change(segment, [{'foo' => 'bar'}], 400, error)
          end # context for #{segment}
        end # [loop over attributes, definitions, files, libraries, providers,
          #              recipes, resources, root_files, templates
      end # context for collections

      context "for other stuff" do
        should_change('frozen?', true)
      end # context for other stuff
    end # context when modifying data

    context "when modifying metadata" do
      should_fail_to_change('metadata', {'new_name' => 'foo'}, 400, "Field 'metadata.version' missing")

      context "for name" do
        should_change_metadata('name', 'new_name', nil, 200, :validation)
        should_change_metadata('name', :delete)
        [[1, 'number'], [true, 'boolean'], [{}, 'object'],
        [[], 'array']].each do |error|
          json_error = "Field 'metadata.name' invalid"
          should_fail_to_change_metadata('name', error[0], 400, json_error)
        end
        ['invalid name', 'ダメよ'].each do |name|
          should_fail_to_change_metadata('name', name, 400, "Field 'metadata.name' invalid")
        end
      end # context for name

      context "for description" do
        should_change_metadata('description', 'new description')
        should_change_metadata('description', :delete)
        should_fail_to_change_metadata('description', 1, 400, "Field 'metadata.description' invalid")
      end # context for description

      context "for long description" do
        should_change_metadata('long_description', 'longer description')

        # Deleting the long description results in it being "reset" to
        # the empty string
        should_change_metadata('long_description', :delete, "")
        should_fail_to_change_metadata('long_description', false, 400, "Field 'metadata.long_description' invalid")
      end # context for long description

      context "for version" do
        should_fail_to_change_metadata('version', '0.0', 400, "Field 'metadata.version' invalid")
        should_fail_to_change_metadata('version', 'not a version', 400, "Field 'metadata.version' invalid")
        should_fail_to_change_metadata('version', :delete, 400, "Field 'metadata.version' missing")
        should_fail_to_change_metadata('version', 1, 400, "Field 'metadata.version' invalid")
      end # context for version

      context "for maintainer" do
        should_change_metadata('maintainer', 'Captain Stupendous')
        should_change_metadata('maintainer', :delete)
        should_fail_to_change_metadata('maintainer', true, 400, "Field 'metadata.maintainer' invalid")
        should_change_metadata('maintainer_email', 'cap@awesome.com')
        should_change_metadata('maintainer_email', 'not really an email')
        should_change_metadata('maintainer_email', :delete)
        should_fail_to_change_metadata('maintainer_email', false, 400, "Field 'metadata.maintainer_email' invalid")
      end # context for maintainer

      context "for license" do
        should_change_metadata('license', 'to_kill')
        should_change_metadata('license', :delete)
        should_fail_to_change_metadata('license', 1, 400, "Field 'metadata.license' invalid")
      end # context for license

      context "for collections" do
        context "for platforms" do
          json_error = "Field 'metadata.platforms' invalid"
          should_fail_to_change_metadata('platforms', [], 400, json_error)
          should_change_metadata('platforms', {})
          should_change_metadata('platforms', :delete)
          should_fail_to_change_metadata('platforms', "foo", 400, json_error)
          should_fail_to_change_metadata('platforms', ["foo"], 400, json_error)
          should_fail_to_change_metadata('platforms', {"foo" => {}}, 400, "Invalid value '{[]}' for metadata.platforms")
        end

        def self.should_change_with_metadata(_attribute, _value)
          context "when #{_attribute} is set to #{_value}" do
            let(:cookbook_name) { Pedant::Utility.with_unique_suffix("pedant-cookbook") }
            # These macros need to be refactored and updated for flexibility.
            # The cookbook endpoint uses PUT for both create and update, so this
            # throws a monkey wrench into the mix.
            should_change_metadata _attribute, _value, _value, 200
          end
        end

        context "with metadata.providing" do
          # In erchef, we are not validating the "providing" metadata
          # See: http://tickets.opscode.com/browse/CHEF-3976

          # http://docs.opscode.com/config_rb_metadata.html#provides
          should_change_with_metadata 'providing', 'cats::sleep'
          should_change_with_metadata 'providing', 'here(:kitty, :time_to_eat)'
          should_change_with_metadata 'providing', 'service[snuggle]'
          should_change_with_metadata 'providing', ''
          should_change_with_metadata 'providing', 1
          should_change_with_metadata 'providing', true
          should_change_with_metadata 'providing', ['cats', 'sleep', 'here']
          should_change_with_metadata 'providing',
            { 'cats::sleep'                => '0.0.1',
              'here(:kitty, :time_to_eat)' => '0.0.1',
              'service[snuggle]'           => '0.0.1'  }

        end

        context "for groupings" do
          json_error = "Field 'metadata.groupings' invalid"
          should_fail_to_change_metadata('groupings', [], 400, json_error)
          should_change_metadata('groupings', {})
          should_change_metadata('groupings', :delete)
          should_fail_to_change_metadata('groupings', "foo", 400, json_error)
          should_fail_to_change_metadata('groupings', ["foo"], 400, json_error)
          should_change_metadata('groupings', {"foo" => {}})
        end # context for groupings

        ['dependencies', 'recommendations', 'suggestions', 'conflicting',
         'replacing'].each do |type|
          context "for #{type}" do
            json_error = "Field 'metadata.#{type}' invalid"
            should_fail_to_change_metadata(type, [], 400, json_error)
            should_change_metadata(type, {})

            if type == "dependencies"
              # Attempting to delete dependencies will result in it
              # getting set to an empty hash, since we need to have
              # something present for that key
              should_change_metadata(type, :delete, {})
            else
               should_change_metadata(type, :delete)
            end

            should_fail_to_change_metadata(type, "foo", 400, json_error)
            should_fail_to_change_metadata(type, ["foo"], 400, json_error)
            should_fail_to_change_metadata(type, {"foo" => {}}, 400, "Invalid value '{[]}' for metadata.#{type}")
          end # context for #{type}
        end # [loop over dependencies, recommendations, suggestions,
            #            conflicting, replacing]
      end # context for collections
    end # context when modifying metadata
  end # context PUT /cookbooks/<name>/<version> [update]
end

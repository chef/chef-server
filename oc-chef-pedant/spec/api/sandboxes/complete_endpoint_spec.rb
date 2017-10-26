# Copyright: Copyright (c) 2012 Opscode, Inc.
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

describe "Sandboxes API Endpoint", :sandboxes do
  include Pedant::RSpec::CookbookUtil

  let(:request_url)    { api_url "/sandboxes" }
  let(:requestor)      { admin_user }

  let(:files) { (1..2).to_a.map { Pedant::Utility.new_random_file } }
  let(:checksums) { files.map { |f| Pedant::Utility.checksum(f) } }
  let(:sandbox_id) { parse(response)["sandbox_id"] }

  # Just a dumb regex to do minimal checking of the URLs we get back
  # for uploading checksums.  If we're using local storage of files,
  # then we can programmatically construct URLs; we can't do this for
  # S3 storage, though.  And really, we shouldn't have to verify the
  # URLs to that degree anyway, since the rest of the tests are all
  # hypermedia-driven.
  url_regex = /^http[s]?:\/\/.*$/

  describe 'Sandboxes Endpoint, POST' do
    let(:request_method) { :POST }

    context 'when creating a new sandbox' do
      let(:expected_response) { resource_created_exact_response }

      let(:request_payload) do
        {
          "checksums" => {
          checksums[0] => nil,
          checksums[1] => nil }
        }
      end

      let(:created_resource) do
        {
          "sandbox_id" => sandbox_id,
          # Sandbox URIs are always in terms of our API
          "uri" => api_url("/sandboxes/#{sandbox_id}"),
          "checksums" => {
            checksums[0] => {
            # URLs might be for local
            # storage, or for S3 (at
            # Amazon or elsewhere).
            # We're just going to do a
            # basic check that they're
            # actually URLs
              "url" => url_regex,
              "needs_upload" => true },
            checksums[1] => {
              "url" => url_regex,
              "needs_upload" => true }
          }
        }
      end

      it 'should respond with 201 Created', :smoke do
        checksums[0].should_not eq checksums[1]
        response.should look_like expected_response
      end
    end

    context 'when creating an invalid sandbox', :validation do
      context 'with an empty request hash' do
        let(:request_payload){ {} }
        it 'should fail' do
          should look_like({
                             :status => 400,
                             :body_exact => { "error" => ["Field 'checksums' missing"] }
                           })
        end
      end

      context 'with an empty checksums hash' do
        let(:request_payload){ {"checksums" => {}} }
        it 'should fail' do
          should look_like ({
                              :status => 400,
                              :body_exact => {
                                "error" =>["Bad checksums!"]
                              }
                            })
        end
      end

      context 'with non-null hash values' do
        let(:request_payload){ {"checksums" => {checksums[0] => "foo"} }}
        it 'should fail' do
          should look_like ({
                              :status => 400,
                              :body_exact => {
                                "error" => ["Bad checksums!"]
                              }
                            })
        end
      end
    end

    context 'with existing file' do
      let(:expected_response) { resource_created_exact_response }
      let(:request_payload) { Pedant::Sandbox.create_payload([new_file, existing_file]) }

      let(:created_resource) do
        {
          "sandbox_id" => sandbox_id,
          "uri" => api_url("/sandboxes/#{sandbox_id}"),
          "checksums" => {
            new_file_checksum => {
              "url" => url_regex,
              "needs_upload" => true },
            existing_file_checksum => {
              # no URL key if it's already been uploaded
              "needs_upload" => false }
          }
        }
      end

      let(:existing_file)          { files[1] }
      let(:existing_file_checksum) { checksums[1] }
      let(:new_file)               { files[0] }
      let(:new_file_checksum)      { checksums[0] }

      # Create a new sandbox with an existing file on disk
      let(:assume_existing_sandbox) do
        create_sandbox([existing_file]).tap do |sandbox|
          upload_to_sandbox(existing_file, sandbox)
          puts "[Sleeping to allow S3 to work ... ]" and sleep 2
          commit_sandbox(sandbox)
        end
      end


      it 'should recognize files that are already on the server' do
        assume_existing_sandbox
        response.should look_like expected_response
      end
    end

    it 'should actually require checksums to create a sandbox', :cleanup do
      pending 'Fix this in Erchef' do
        post(api_url("/sandboxes"),
             admin_user,
             :payload => {"checksums" => {}}) do |response|
          response.should look_like({
                                      :status => 400,
                                      :error => ["Missing checksums!"]
                                    })
        end
      end
    end

    it 'should require valid checksums to create a sandbox', :cleanup do
      pending 'Fix this in Erchef' do
        post(api_url("/sandboxes"),
             admin_user,
             :payload => {
               "checksums" => {
                 "Not-A-Checksum-----$@%@#!" => nil
               }}) do |response|
          response.should look_like({
                                      :status => 400,
                                      :error => ["Invalid checksum!"]
                                    })
        end
      end
    end

    respects_maximum_payload_size
  end

  describe 'Sandboxes Endpoint, PUT' do
    let(:request_method) { :PUT }
    let(:request_url)    { sandbox["uri"] } # Use uri returned by sandbox

    let(:file1) { files[0] }
    let(:file2) { files[1] }
    let(:dummy_file) { Pedant::Utility.new_random_file }

    let(:sandbox) { create_sandbox(files) }
    let(:sandbox_id) { sandbox['sandbox_id'] }
    let(:error_sums) { files.map{ |f| Pedant::Utility.checksum(f) }.sort }

    context 'when committing an incomplete sandbox' do
      let(:expected_response) { { status: 503 } }
      let(:request_payload) { { "is_completed" => true } }

      let(:error_message) do
        ["Cannot update sandbox #{sandbox_id}: the following checksums have not been uploaded: #{error_sums.join(', ')}"]
      end

      should_respond_with 503
    end

    context 'when uploading expected files to the sandbox ' do
      let(:response) { upload_to_sandbox(file1, sandbox) }

      it 'should respond with 200 OK or 204 No Content' do
        should look_like({:status => [200, 204]})
      end

      it 'response headers should not contain :content_length when response code is 204' do
        response2 = upload_to_sandbox(file1, sandbox)
        # Protect in case the response is a 200 e.g from chef-zero
        # TODO: Fix chef-zero to rerurn a 204 in this situation instead of 200
        if response2.code == 204
          response2.code.should eq(204)
          response2.headers.should_not include(:content_length)
        end
      end
    end

    context 'when committing a sandbox after uploading files' do
      let(:expected_response) { ok_full_response }
      let(:request_payload) { { "is_completed" => true } }

      let(:responses_from_upload) { files.map { |f| upload_to_sandbox f, sandbox } }
      let(:success_message) do
        {
          "guid"        => sandbox_id,
          "name"        => sandbox_id,
          "checksums"   => checksums,
          "create_time" => timestamp_regexp
        }
      end

      # YYYY-MM-DDT00:00:00+00:00, but we'll constraint it to at least today's date
      let(:timestamp_regexp) { Regexp.new "#{platform.now.strftime('%Y-%m-%d')}T\\d\\d:\\d\\d:\\d\\d\[\\+\\-\]\\d\\d:\\d\\d" }

      it 'should respond with 200 OK', :smoke do
        # Upload and check files
        responses_from_upload.each { |r| r.should look_like({:status => [200, 204]}) }

        # Signal that the sandbox is complete
        response.should look_like expected_response
        parsed_response.should have_key 'is_completed'
      end
    end

    it 'erroneously reports a file is not uploaded when trying to commit an already committed sandbox', :cleanup do
      pending 'fix this in Erchef' do
        file1 = Pedant::Utility.new_random_file
        checksum = Pedant::Utility.checksum(file1)

        sandbox = create_sandbox([file1])
        sandbox_id = sandbox["sandbox_id"]
        upload_to_sandbox(file1, sandbox).should look_like({
                                                             :status => 200,
                                                             :body_exact => {
                                                               "uri" => sandbox["checksums"][checksum]["url"]
                                                             }
                                                           })

        commit_sandbox(sandbox).should look_like({
                                                   :status => 200,
                                                   :body => {
                                                     "guid" => sandbox_id,
                                                     "checksums" => [checksum]
                                                   }
                                                 })
        r = commit_sandbox(sandbox)
        r.should have_status_code 400
        json = parse(r)

        # Yes it was uploaded... we just did that
        json["error"].should_not eq ["Cannot update sandbox #{sandbox_id}: checksum #{checksum} was not uploaded"]

      end
    end

    # 'is_completed' isn't actually set to 'true' in the Ruby endpoint
    it "'is_committed' should be true for a committed sandbox" do
      file1 = Pedant::Utility.new_random_file
      file2 = Pedant::Utility.new_random_file

      sandbox = create_sandbox([file1, file2])
      sandbox_id = sandbox["sandbox_id"]
      [file1, file2].each {|f| upload_to_sandbox(f, sandbox)}
      commit_sandbox(sandbox).should look_like({
                                                 :status => 200,
                                                 :body => {
                                                   # sanity checks
                                                   "guid" => sandbox_id,
                                                   "checksums" => [file1, file2].map{|f| Pedant::Utility.checksum(f)},
                                                   # This is the real test
                                                   "is_completed" => true
                                                 }
                                               })
    end

    it "should not leak CouchDB '_rev' fields after committing a sandbox", :cleanup do
      pending "Fix this in Erchef" do
        file1 = Pedant::Utility.new_random_file
        file2 = Pedant::Utility.new_random_file

        sandbox = create_sandbox([file1, file2])
        [file1, file2].each {|f| upload_to_sandbox(f, sandbox)}

        r = commit_sandbox(sandbox)
        r.should have_status_code 200

        json = parse(r)
        json.should_not have_key "_rev"

      end

    end

    respects_maximum_payload_size
  end
end

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

require 'pedant/concern'
require 'rspec/core/shared_context'
require 'rspec-shared/methods'

module Pedant
  module RSpec
    module CookbookUtil
      extend ::RSpec::Core::SharedContext
      extend ::RSpecShared::Methods
      extend Pedant::Concern

      def authenticated_request(method, url, *args, &block)
        response = nil
        if block_given?
          super(method, url, *args) { |r| response = r; block.call(r) }
        else
          response = super(method, url, *args)
        end
        # Anyone who does a PUT to cookbooks gets automagically added to created_server_objects
        if method == :PUT && url =~ /(\/(cookbooks|cookbook_artifacts)\/[^\/]*\/[^\/]*)$/
          if response.code.to_s == "201"
            created_server_objects << $1
          end
        end
        response
      end

      # The list of things we've created throughout this test
      let(:created_server_objects) { [] }
      after do
        # Delete the objects in the reverse order they were created
        created_server_objects.uniq.reverse_each do |path|
          response = delete(path)
          # It's OK if they already exist, but not OK if the delete *fails*
          expect(response.code.to_s).to eq('200').or eq('404')
        end
      end

      # Sandbox utils live here as well
      # When the request methods get pulled out into external modules,
      # these can go into Pedant::Sandbox

      # pedant_files should be things in PEDANT_ROOT/files
      # Returns the entire sandbox JSON as a Ruby hash
      def create_sandbox(files)
        payload = Pedant::Sandbox.create_payload(files)
        parse(ensure_2xx(post(api_url("/sandboxes"),
                              admin_user,
                              :payload => payload)))
      end

      # Uploads the given file to the sandbox.  Assumes that the sandbox
      # exists and is expecting the file to be uploaded.
      def upload_to_sandbox(file, sandbox)

        checksum = Pedant::Utility.checksum(file)
        base64 = Pedant::Utility.base64_checksum(checksum)
        headers = {
          'content-type' => 'application/x-binary',
          'content-md5' => base64,
        }

        if sandbox["checksums"][checksum]["needs_upload"]
          upload_url = sandbox["checksums"][checksum]["url"]
          # file is an unclosed File object returned from our utility
          # functions that make use of Tempfile. Since we want the
          # entire contents, rewind before read.
          file.rewind
          ensure_2xx(put(upload_url, admin_user, :payload => file.read,
                         :headers => headers))
        else
          true
        end
      end

      def commit_sandbox(sandbox)
        ensure_2xx(put(sandbox["uri"], nil, payload: { "is_completed" => true }))
      end

      def upload_files_to_sandbox(files)
        if files.length > 0
          box = create_sandbox(files)
          files.each { |f| upload_to_sandbox(f, box) }
          commit_sandbox(box)
        end
        files
      end

      # Creates a random file and uploads it to the organization.  Returns
      # the checksum of that file.
      def generate_dummy_checksum()
        file = Pedant::Utility.new_random_file
        checksum = Pedant::Utility.checksum(file)
        sandbox = create_sandbox([file])
        upload_to_sandbox(file, sandbox)
        sleep 2 #give s3 some time
        commit_sandbox(sandbox)
        checksum
      end

      # Do not put these lets in included() as this will break (override ordering)
      # Concern is included here to make it easy to extend the example group using
      # module ClassMethods


      let(:cookbook_version_not_found_exact_response) do
        {
          :status => 404,
          :body_exact => { "error" => cookbook_version_not_found_error_message }
        }
      end
      let(:cookbook_version_not_found_error_message) { ["Cannot find a cookbook named #{cookbook_name} with version #{cookbook_version}"] }
      let(:cookbook_not_found_error_message) { ["Cannot find a cookbook named #{cookbook_name}"] }

      let(:invalid_cookbook_version_response) do
        {
          :status => 400,
          :body => { "error" => invalid_cookbook_version_error_message }
        }
      end

      let(:invalid_cookbook_version_exact_response) do
        {
          :status => 400,
          :body_exact => { "error" => invalid_cookbook_version_error_message }
        }
      end

      let(:delete_invalid_cookbook_version_error_message) { invalid_cookbook_version_error_message }

      let(:invalid_versions_msg) {
        ["You have requested an invalid number of versions (x >= 0 || 'all')"] }

      let(:fetch_cookbook_success_exact_response) do
        {
          :status => 200,
          :body => fetched_cookbook
        }
      end
      let(:fetch_cookbook_not_found_exact_response) do
        {
          :status => 404,
          :body_exact => { "error" => cookbook_not_found_error_message }
        }
      end

      let(:delete_cookbook_success_response) { { status: 200 } }
      # Assertion Helpers

      # Verify update did not happen
      let(:should_not_be_updated) { should_not_be_deleted }

      # Verify delete did happen
      let(:should_be_deleted) do
        get(request_url, admin_user) do |response|
          response.should look_like cookbook_version_not_found_exact_response
        end
      end

      # Verify delete did not happen
      let(:should_not_be_deleted) do
        get(request_url, admin_user) do |response|
          response.should look_like fetch_cookbook_success_exact_response
        end
      end

      def new_cookbook_artifact(name, identifier, opts = {})
        {
          "name" => "#{name}",
          "identifier" => identifier,
          "version" => opts[:version] || default_version, # version doesn't matter for cookbook_artifacts
          "json_class" => "Chef::CookbookVersion",
          "chef_type" => "cookbook_version",
          "frozen?" => false,
          "recipes" => opts[:recipes] || [],
          "metadata" => {
            "version" => opts[:version] || default_version,
            "name" => name, # not actually used
            "maintainer" => opts[:maintainer] || default_maintainer,
            "maintainer_email" => opts[:maintainer_email] || default_maintainer_email,
            "description" => opts[:description] || default_description,
            "long_description" => opts[:long_description] || default_long_description,
            "license" => opts[:license] || default_license,
            "dependencies" => opts[:dependencies] || {},
            "attributes" => opts[:attributes] || {},
            # this recipies list is not the same as the top level list
            # this is a list of recipes and their descriptions
            "recipes" => opts[:meta_recipes] || {}
          }
        }
      end

      def make_cookbook_artifact(path, opts = {})
        payload = new_cookbook_artifact(name, identifier, opts)
        res = upload_cookbook(path, payload)
        expect(res.code).to eq(201)
      end

      def make_cookbook_artifact_with_recipes(path, recipe_list)
        recipe_specs = normalize_recipe_specs(recipe_list)
        content_list = recipe_specs.map { |r| r[:content] }
        files = content_list.map { |content| Pedant::Utility.new_temp_file(content) }
        upload_files_to_sandbox(files)
        checksums = files.map { |f| Pedant::Utility.checksum(f) }
        recipes = recipe_specs.zip(checksums).map do |r, sum|
          dummy_recipe(r[:name], sum)
        end.sort { |a, b| a[:name] <=> b[:name] }
        opts = { :recipes => recipes }
        make_cookbook_artifact(path, opts)
      end

      def get_cookbook_checksums(path)
        # Cookbook looks like:
        # {
        #   "name": "cookbook",
        #   ...
        #   "recipes": [
        #     { "name": "default.rb", "path": "recipes/default.rb", "checksum": "13984723984723", "url": "https://s3.com/path/to/file" },
        #     ...
        #   ],
        #   "libraries": [
        #     { "name": "default.rb", "path": "recipes/default.rb", "checksum": "13984723984723", "url": "https://s3.com/path/to/file" },
        #     ...
        #   ],
        #   ...
        # }
        # We are going to go through and grab everything with a checksum.
        result = {}
        parse(get(path)).each_pair do |segment, files|
          if files.is_a?(Array)
            files.each do |file|
              if file['checksum']
                result[file['checksum']] = file['url']
              end
            end
          end
        end
        result
      end


      # Verifies all deleted checksums are properly removed from Bookshelf (or S3).
      # The sets of 'existing' and 'updated' checksums can either be pre-computed
      # and passed in as function arugments or automatically computed if a
      # block is provided.
      def verify_checksum_cleanup(path, &block)
        existing_checksums ||= get_cookbook_checksums(path)

        yield if block_given?

        updated_checksums ||= get_cookbook_checksums(path)

        deletions = existing_checksums.keys - updated_checksums.keys

        # If any of these checks fail we *may* be experiencing eventual
        # consistency.  Consider adding an evil sleep.
        deletions.each do |checksum|
          verify_checksum_url(existing_checksums[checksum], 404)
        end
      end

      def verify_checksum_url(url, expected_reponse_code)
        uri = URI.parse(url)
        http = Net::HTTP.new(uri.hostname, uri.port)
        if uri.scheme == 'https'
          http.use_ssl = true
          http.verify_mode = OpenSSL::SSL::VERIFY_NONE
        end

        response = http.get(uri.request_uri, {})

        begin
          response.code.should eq expected_reponse_code.to_s
        rescue => e # let's rescue and raise with a useful error message
          raise e, " Expected a #{expected_reponse_code} when performing a GET to "\
            << " [#{url}] but received a #{response.code}."

        end
      end

      def make_cookbook(path, opts={})
        name, version = path.split("/")[2..3]
        payload = new_cookbook(name, version, opts)
        upload_cookbook(path, payload)
      end

      def upload_cookbook(path, payload)
        put(path, nil, :payload => payload)
      end

      def new_cookbook(name, version, opts = {})
        {
          "name" => "#{name}-#{version}",
          "cookbook_name" => name,
          "version" => version, # not actually used
          "json_class" => "Chef::CookbookVersion",
          "chef_type" => "cookbook_version",
          "frozen?" => false,
          "recipes" => opts[:recipes] || [],
          "metadata" => {
            "version" => version,
            "name" => name, # not actually used
            "maintainer" => opts[:maintainer] || default_maintainer,
            "maintainer_email" => opts[:maintainer_email] || default_maintainer_email,
            "description" => opts[:description] || default_description,
            "long_description" => opts[:long_description] || default_long_description,
            "license" => opts[:license] || default_license,
            "dependencies" => opts[:dependencies] || {},
            "attributes" => opts[:attributes] || {},
            # this recipies list is not the same as the top level list
            # this is a list of recipes and their descriptions
            "recipes" => opts[:meta_recipes] || {}
          }
        }
      end

      shared(:default_description) { "A fabulous new cookbook" }
      shared(:default_long_description) { "" }
      shared(:default_maintainer) { "Your Name" }
      shared(:default_maintainer_email) { "youremail@example.com" }
      shared(:default_license) { "Apache v2.0" }

      def full_cookbook(name, version, opts = {})
        {
          "name" => "#{name}-#{version}",
          "cookbook_name" => name,
            "version" => version,
            "json_class" => "Chef::CookbookVersion",
            "chef_type" => "cookbook_version",
            "recipes" => opts[:recipes] || [],
            "metadata" => {
            "name" => name,
            "description" => opts[:description] || default_description,
            "long_description" => opts[:long_description] || default_long_description,
            "maintainer" => opts[:maintainer] || default_maintainer,
            "maintainer_email" => opts[:maintainer_email] || default_maintainer_email,
            "license" => opts[:license] || default_license,
            "platforms" => {},
            "dependencies" => {},
            "recommendations" => {},
            "suggestions" => {},
            "conflicting" => {},
            "providing" => {},
            "replacing" => {},
            "attributes" => {},
            "groupings" => {},
            "recipes" => opts[:meta_recipes] || {},
            "version" => version
          },
            "frozen?" => opts[:frozen] || false
        }
      end

      # We don't return all the metadata when fetching a cookbook via
      # the API because it's not used by the client and wastes
      # bandwidth
      def retrieved_cookbook(name, version, opts = {})
        cookbook = {
          "name" => "#{name}-#{version}",
          "version" => version,
          "cookbook_name" => name,
          "json_class" => "Chef::CookbookVersion",
          "chef_type" => "cookbook_version",
          "frozen?" => opts[:frozen] || false,
          "recipes" => opts[:recipes] || []
        }

        metadata = {
          "attributes" => {},
          "dependencies" => {},
          "description" => opts[:description] || default_description,
          "license" => opts[:license] || default_license,
          "long_description" => opts[:long_description] || default_long_description,
          "maintainer" => opts[:maintainer] || default_maintainer,
          "maintainer_email" =>  opts[:maintainer_email] || default_maintainer_email,
          "name" => name,
          "recipes" => opts[:meta_recipes] || {},
          "version" => version
        }

        cookbook["metadata"] = metadata
        cookbook
      end

      # Create a dummy recipe for a cookbook recipe manifest.  The
      # checksum is assumed to already exist in the organization.
      def dummy_recipe(name, checksum)
        {
          "name" => "#{name}.rb",
          "path" => "recipes/#{name}.rb",
          "checksum" => checksum,
            "specificity" => "default",
        }
      end

      def normalize_recipe_specs(recipe_specs)
        recipe_specs.map do |spec|
          if spec.is_a?(String)
            {
              :name => spec,
              :content => "pedant-recipe-content-#{unique_suffix}"
            }
          else
            spec
          end
        end
      end

      # Saves a dummy cookbook with recipes.  Recipes can be specified
      # either as a string in which case a file with random content
      # will be created and uploaded or as a "recipe spec", a hash
      # with keys `:name` and `:content`, where the content key is a
      # string that will be used as the recipe file contents.
      #
      # TODO - but no dependency on ruby flag, so...
      # NOTE: Recipe names are sorted here to accommodate differences
      # between the Ruby and Erlang implementations; when retrieving
      # recipe names (e.g. /environments/ENVIRONMENT/recipes), the results
      # are sorted alphabetically; this is due to Ruby sorting the recipe
      # names *when the cookbook is created*, rather than sorting when the
      # data is returned.  The Erlang behavior is the opposite.
      def save_dummy_cookbook_with_recipes(path, recipe_list)
        recipe_specs = normalize_recipe_specs(recipe_list)
        content_list = recipe_specs.map { |r| r[:content] }
        files = content_list.map { |content| Pedant::Utility.new_temp_file(content) }
        upload_files_to_sandbox(files)
        checksums = files.map { |f| Pedant::Utility.checksum(f) }
        recipes = recipe_specs.zip(checksums).map do |r, sum|
          dummy_recipe(r[:name], sum)
        end.sort { |a, b| a[:name] <=> b[:name] }
        opts = { :recipes => recipes }
        make_cookbook(path, opts)
      end

      # Generate and insert cookbooks based on the following format
      #
      # { "/cookbooks/my_cookbook/1.0.0" => [ "recipe1", "recipe2"],
      #   "/cookbooks/my_cookbook/2.0.0" => [ "recipe3", "recipe4" ],
      #   "/cookbooks/your_cookbook/0.0.1" => [ "recipe1", "recipe2"],
      #   "/cookbooks/your_cookbook/1.5.0" => [ "recipe3", "recipe4" ] }
      #
      def setup_cookbooks(cookbook_spec)
        cookbook_spec.each do |path, recipe_names|
          save_dummy_cookbook_with_recipes(path, recipe_names)
        end
      end

      # Delete the specified cookbook versions.  Given the same argument hash, this will remove
      # everything created by +setup_cookbooks+
      def remove_cookbooks(cookbook_spec)
        cookbook_spec.each_key { |path| delete(path) }
      end

      # Sort versions in descending order and only pick the top n versions for
      # each cookbook.
      # { "/cookbooks/apache2/1.0.1" => [ ... ], ... }
      def get_latest_cookbooks(cookbook_spec, num_versions=1)
        results = {}
        cookbook_spec.group_by { |path,recipes| path.split('/')[0..-2].join('/') }.each_pair do |cookbook, versions|
          # Right now this just sorts lexicographically, which works fine
          # when major, minor, and patch numbers are all single-digit
          versions = versions.sort_by { |path,recipes| path }.reverse
          versions = versions.take(num_versions) unless num_versions == 'all'
          versions.each { |k,v| results[k] = v }
        end
        results
      end

      module ClassMethods

        # This is used for testing creates with changes to the default
        # new_cookbook; One key/value pair is changed/added and the result
        # checked
        #   key:     key to change
        #   value:   value to use
        def should_create(key, value, ignores_value = false, actual_value = nil, metadata = {})
          # create and update are identical; both use PUT
          should_change(key, value, ignores_value, actual_value, true, metadata)
        end

        # This is used for testing updates with changes to the default
        # new_cookbook; One key/value pair is changed/added and the result
        # checked -- but the actual_value is expected instead of the value
        # passed when the result is checked
        #   key:     key to change
        #   value:   value to use
        def should_not_change(key, value, actual_value)
          should_change(key, value, true, actual_value)
        end

        # This is used for testing updates with changes to the default
        # new_cookbook; One key/value pair is changed/added and the result
        # checked
        #   key:           key to change
        #   value:         value to use
        #   ignores_value: if the sent value doesn't matter and is reset
        # actual_value shouldn't normally ever be passed -- use
        # should_not_change_data instead (possibly same for ignores_value as well)
        # create shouldn't ever be passed, use should_create instead
        def should_change(key, value, ignores_value = false, actual_value = nil,
                          create = false, metadata = {})
          it "#{key} = #{value} returns 200", metadata do
            payload = new_cookbook(cookbook_name, cookbook_version)
            if (value == :delete)
              payload.delete(key)
            else
              payload[key] = value
            end

            path = "/cookbooks/#{cookbook_name}/#{cookbook_version}"
            response = upload_cookbook(path, payload)
            if ignores_value
              payload[key] = actual_value
            end
            expect(response).to look_like(status: create ? 201 : 200, body_exact: payload)

            # Verified change (or creation) happened
            response = get(path)
            expect(response).to look_like(status: 200, body: payload)
          end
        end

        # This is used when creation is expected to fail; the key/value pair
        # is added/modified in the default new_cookbook, but the error and message
        # are expected instead of 200 (success) and a normal payload -- verifies
        # that nothing is created as well
        #   key:     key to change
        #   value:   value to use
        #   error:   expected HTTP error code
        #   message: error message expected
        def should_fail_to_create(key, value, error, message)
          # Create and update are identical; both use PUT
          should_fail_to_change(key, value, error, message, false, true)
        end

        # This is used when the update operation is expected to fail; the
        # key/value pair is added/modified in the default new_cookbook, but
        # the error and message are expected instead of 200 (success) and a
        # normal payload -- verifies that nothing is modified as well
        #   key:     key to change
        #   value:   value to use
        #   error:   expected HTTP error code
        #   message: error message expected
        # create and server_error shouldn't normally ever be passed, use
        # other functions instead
        def should_fail_to_change(key, value, error, message, server_error = false,
                                  create = false)
          tags = []
          tags << :validation if error == 400
          it "#{key} = #{value} returns #{error}", *tags do
            payload = new_cookbook(cookbook_name, cookbook_version)
            if (value == :delete)
              payload.delete(key)
            else
              payload[key] = value
            end

            path = "/cookbooks/#{cookbook_name}/#{cookbook_version}"
            response = upload_cookbook(path, payload)
            if server_error
              expect(response).to match(/^HTTP\/1.1 500 Internal Server Error/)
            else
              expect(response).to look_like(status: error, body_exact: { "error" => [ message ] })
            end

            # Verified change (or creation) did not happen
            if create
              response = get(path)
              expect(response).to look_like(status: 404)
            else
              response = get(path)
              payload = new_cookbook(cookbook_name, cookbook_version)
              expect(response).to look_like(status: 200, body_exact: payload)
            end
          end
        end

        # This is used for testing creates with changes to the default
        # new_cookbook; One key/value pair is changed/added in the metadata
        # and the result checked
        #   key:     key to change
        #   value:   value to use
        #   tags:    tags to add to the test
        def should_create_metadata(key, value, *tags)
          should_change_metadata(key, value, nil, 200, *tags)
        end

        # This is used for testing updates with changes to the default
        # new_cookbook; One key/value pair is changed/added in the metadata
        # and the result checked -- in this case, however, the new_value is
        # expected instead of the original passed value
        #   key:       key to change
        #   value:     value to pass
        #   new_value: expected value for key
        #   tags: tags to add to the test
        def should_not_change_metadata(key, value, new_value, *tags)
          should_change_metadata(key, value, new_value, 200, *tags)
        end

        # This is used for testing updates with changes to the default
        # new_cookbook; One key/value pair is changed/added in the metadata
        # and the result checked
        #   key:     key to change
        #   value:   value to use
        # new_value shouldn't normally ever be passed -- use
        # should_mot_change_metadata instead
        def should_change_metadata(key, value, new_value = nil, _expected_status = 200, *tags)
          it "#{key} = #{value} returns #{_expected_status}", *tags do

            cookbook = new_cookbook(cookbook_name, cookbook_version)

            put_payload = cookbook.dup
            put_metadata = put_payload["metadata"]
            if value == :delete
              put_metadata.delete(key)
            else
              put_metadata[key] = value
            end
            put_payload["metadata"] = put_metadata

            path = "/cookbooks/#{cookbook_name}/#{cookbook_version}"
            response = upload_cookbook(path, put_payload)
            # The PUT response returns the payload exactly as it was sent
            expect(response).to look_like(status: _expected_status, body_exact: put_payload)

            # Verified change (or creation) happened
            get_response = cookbook.dup
            if new_value
              get_metadata = get_response["metadata"]
              get_metadata[key] = new_value
              get_response["metadata"] = get_metadata
            end

            response = get(path)
            expect(response).to look_like(status: 200, body: get_response)
          end
        end

        # This is used for testing creates with changes to the default
        # new_cookbook; One key/value pair is changed/added in the metadata,
        # but the error and message are expected back instead of a 200
        # (SUCCESS) and normal payload -- also verifies no changes were made
        # to the cookbook
        #   key:     key to change
        #   value:   value to use
        #   error:   expected HTTP error code
        #   message: error message expected
        def should_fail_to_create_metadata(key, value, error, message)
          should_fail_to_change_metadata(key, value, error, message, true)
        end

        # This is used for testing updates with changes to the default
        # new_cookbook; One key/value pair is changed/added in the metadata,
        # but the error and message are expected back instead of a 200
        # (SUCCESS) and normal payload -- also verifies no changes were made
        # to the cookbook
        #   key:     key to change
        #   value:   value to use
        #   error:   expected HTTP error code
        #   message: error message expected
        # create and server_error shouldn't normally ever be passed, use
        # other functions instead
        def should_fail_to_change_metadata(key, value, error, message, create = false,
                                           server_error = false)
          tags = []
          tags << :validation if error == 400
          it "#{key} = #{value} returns #{error}", *tags do
            payload = new_cookbook(cookbook_name, cookbook_version)
            metadata = payload["metadata"]
            if (value == :delete)
              metadata.delete(key)
            else
              metadata[key] = value
            end
            payload["metadata"] = metadata
            response = upload_cookbook(path, payload)
            if server_error
              expect(response).to match(/^HTTP\/1.1 500 Internal Server Error/)
            else
              expect(response).to look_like(status: error, body_exact: { "error" => [message] })
            end

            # Verified change (or creation) did not happen
            path = "/cookbooks/#{cookbook_name}/#{cookbook_version}"
            if create
              response = get(path)
              expect(response).to look_like(status: 404)
            else
              payload = new_cookbook(cookbook_name, cookbook_version)
              response = get(path)
              expect(response).to look_like(status: 200, body_exact: payload)
            end
          end
        end
      end # module Class Methods

    end # CookbookUtils
  end # RSpec
end # Pedant

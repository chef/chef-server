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

describe "Cookbooks API endpoint", :cookbooks, :cookbooks_delete do

  include Pedant::RSpec::CookbookUtil

  context "DELETE /cookbooks/<name>/<version>" do

    it "deleting a non-existent cookbook 404s" do
      expect( delete("/cookbooks/non-existent-cookbook/99.99.99") ).to look_like(
        status: 404,
        body_exact: { "error" => ["Cannot find a cookbook named non-existent-cookbook with version 99.99.99"] }
      )
    end

    it "deleting a cookbook with a poorly formatted version", :validation do
      expect( delete("/cookbooks/non-existent-cookbook/1.2.3.4") ).to look_like(status: 400)
    end

    context "when cookbook-to-be-deleted version 1.2.3 has a file in it" do
      before do
        setup_cookbooks(
          "/cookbooks/cookbook-to-be-deleted/1.2.3" => [ { name: "test_recipe", content: "hello-#{unique_suffix}" } ]
        )
      end

      it "deleting cookbook-to-deleted version 1.2.3 returns 200 and deletes the cookbook", :smoke do
        # Grab the checksums
        checksums = get_cookbook_checksums("/cookbooks/cookbook-to-be-deleted/1.2.3")
        expect(checksums.size).to eq(1)

        # Delete the cookbook
        expect( delete("/cookbooks/cookbook-to-be-deleted/1.2.3") ).to look_like(status: 200)

        # Ensure all the checksums' associated files were deleted
        checksums.each_pair do |checksum, uri|
          expect(get(uri)).to look_like(status: 404)
        end
      end

      it "for admin user, delete responds with 200, and the cookbook is removed", :authorization do
        expect( delete("/cookbooks/cookbook-to-be-deleted/1.2.3"), admin_user ).to look_like(status: 200)
        expect( get("/cookbooks/cookbook-to-be-deleted/1.2.3") ).to look_like(status: 404)
      end

      it "for normal user, delete responds with 200, and the cookbook is removed", :authorization do
        expect( delete("/cookbooks/cookbook-to-be-deleted/1.2.3", normal_user) ).to look_like(status: 200)
        expect( get("/cookbooks/cookbook-to-be-deleted/1.2.3") ).to look_like(status: 404)
      end

      it "for a user outside of the org, delete responds with 403, and the cookbook is NOT removed", :authorization do
        expect( delete("/cookbooks/cookbook-to-be-deleted/1.2.3", outside_user) ).to look_like(status: 200)
        expect( get("/cookbooks/cookbook-to-be-deleted/1.2.3") ).to look_like(status: 200)
      end

      it "for an invalid user, delete responds with 401, and the cookbook is NOT removed", :authentication do
        expect( delete("/cookbooks/cookbook-to-be-deleted/1.2.3", invalid_user) ).to look_like(status: 401)
        expect( get("/cookbooks/cookbook-to-be-deleted/1.2.3") ).to look_like(status: 200)
      end

      context "and some-other-cookbook version 2.3.4 has a file with the same content" do
        before do
          setup_cookbooks(
            "/cookbooks/some-other-cookbook/2.3.4" => [ { name: "blarghle", content: "hello-#{unique_suffix}" } ],
          )
        end

        it "deleting cookbook-to-be-deleted version 1.2.3 does not clean up the checksum" do
          # Grab the checksums
          checksums = get_cookbook_checksums("/cookbooks/cookbook-to-be-deleted/1.2.3")
          expect(checksums.size).to eq(1)
          expect(get_cookbook_checksums("/cookbooks/some-other-cookbook/2.3.4")).to eq(checksums)

          # Delete the cookbook
          expect( delete("/cookbooks/cookbook-to-be-deleted/1.2.3") ).to look_like(status: 200)

          # Ensure none of the checksums' associated files were deleted
          checksums.each_pair do |checksum, uri|
            expect(get(uri)).to look_like(status: 200)
          end
        end
      end
    end
  end # context DELETE /cookbooks/<name>/<version>
end

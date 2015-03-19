#
# Copyright 2015 Chef Software, Inc.
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
#

require "omnibus_ctl_helper"
require 'helpers/key_ctl_helper'
require 'chef/rest'

describe "chef-server-ctl *key(s)*" do
  before(:all) do
    @helper = OmnibusCtlHelper.new(["./key_control.rb"])
  end

  # marco for testing mandatory args
  def mandatory_argument_should_exist(command, arg_name, arg_list, arg_number)
    expect { @helper.run_test_omnibus_command(command, arg_list) }.to raise_error(SystemExit, Regexp.new(KeyCtlHelper.new.argument_missing_msg(arg_name, arg_number)))
  end

  let(:public_key) {
    "-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvPo+oNPB7uuNkws0fC02
KxSwdyqPLu0fhI1pOweNKAZeEIiEz2PkybathHWy8snSXGNxsITkf3eyvIIKa8OZ
WrlqpI3yv/5DOP8HTMCxnFuMJQtDwMcevlqebX4bCxcByuBpNYDcAHjjfLGSfMjn
E5lZpgYWwnpic4kSjYcL9ORK9nYvlWV9P/kCYmRhIjB4AhtpWRiOfY/TKi3P2LxT
IjSmiN/ihHtlhV/VSnBJ5PzT/lRknlrJ4kACoz7Pq9jv+aAx5ft/xE9yDa2DYs0q
Tfuc9dUYsFjptWYrV6pfEQ+bgo1OGBXORBFcFL+2D7u9JYquKrMgosznHoEkQNLo
0wIDAQAB
-----END PUBLIC KEY-----"
  }

  let(:public_key_fingerprint) { "12:3e:33:73:0b:f4:ec:72:dc:f0:4c:51:62:27:08:76:96:24:f4:4a" }

  let(:private_key_path) { "#{__dir__}/fixtures/pivotal.pem" }
  let(:public_key_path)  { "#{__dir__}/fixtures/pivotal.pub" }

  let(:bad_public_key_path)  { "#{__dir__}/fixtures/badkey.pub" }
  let(:not_a_key_path)       { "#{__dir__}/fixtures/notakey.pub" }

  before(:each) do
    allow_any_instance_of(KeyCtlHelper).to receive(:pivotal_config).and_return("#{__dir__}/fixtures/pivotal.rb")
  end

  shared_examples_for "a key command with option parsing" do
    context "when an invalid argument is passed" do
      it "should return a proper error" do
        expect { @helper.run_test_omnibus_command(command, ["--wrong"]) }.to raise_error(SystemExit, KeyCtlHelper.new.invalid_arg_msg("--wrong"))
      end
    end
  end

  shared_examples_for "a key command with options that require input" do
    context "when a valid argument is missing valid input" do
      it "should return a proper error" do
        expect { @helper.run_test_omnibus_command(command, [valid_arg_missing_input]) }.to raise_error(SystemExit, KeyCtlHelper.new.missing_valid_input_msg(valid_arg_missing_input))
      end
    end
  end

  shared_examples_for "a key command with multiple options that require input" do
    context "when a command that requires input is passed another command" do
      it "should return a proper error" do
        expect { @helper.run_test_omnibus_command(command, arguments) }.to raise_error(SystemExit, KeyCtlHelper.new.missing_input_msg(argument_followed_by_invalid_input))
      end
    end
  end

  describe "listing key commands" do

    shared_examples_for "a key listing command" do
      it "should output the name and expiry status of the default key" do
        expect { @helper.run_test_omnibus_command(command, arguments) }.to output(/name: default\nexpired: false/).to_stdout
      end
      it "should output the name and expiry status of any other valid key" do
        expect { @helper.run_test_omnibus_command(command, arguments) }.to output(/name: key2\nexpired: true/).to_stdout
      end
      context "when it is called with --show-public-keys" do
        it "should output a public key" do
          expect { @helper.run_test_omnibus_command(command, arguments.concat(["--show-public-keys"])) }.to output(/public_key:\n-----BEGIN PUBLIC KEY-----/).to_stdout
        end
      end
    end

    describe "list-user-keys" do
      before(:each) do
        allow_any_instance_of(KeyCtlHelper).to receive(:get_rest).with("/users/testuser/keys").and_return(
          [
            {"uri"=>"https://127.0.0.1/users/pivotal/keys/default",
             "name"=>"default",
             "expired"=>false},
            {"uri"=>"https://127.0.0.1/users/pivotal/keys/key2",
             "name"=>"key2",
             "expired"=>true}
          ]
        )

        allow_any_instance_of(Chef::REST).to receive(:get_rest).with("https://127.0.0.1/users/pivotal/keys/default").and_return(
          {"name"=>"default", "public_key"=>public_key, "expiration_date"=>"infinity"}
        )
        allow_any_instance_of(Chef::REST).to receive(:get_rest).with("https://127.0.0.1/users/pivotal/keys/key2").and_return(
          {"name"=>"default", "public_key"=>public_key, "expiration_date"=>"2012-01-01T00:00:00Z"}
        )
      end

      it_should_behave_like "a key command with option parsing" do
        let(:command)   { "list-user-keys" }
      end

      it_should_behave_like "a key listing command" do
        let(:command)   { "list-user-keys" }
        let(:arguments) { ["testuser"] }
      end

      context "when valid arguments are passed to list-user-keys" do
        it "should make an http request to /users/testuser/keys" do
          expect_any_instance_of(KeyCtlHelper).to receive(:get_rest).with("/users/testuser/keys").at_least(:once)
          @helper.run_test_omnibus_command("list-user-keys", ["testuser"])
        end
      end
      context "when a mandatory argument is missing to list-user-keys" do
        context "when USERNAME is missing" do
          it "should fail with the proper error message" do
            mandatory_argument_should_exist "list-user-keys", "USERNAME", [], 1
          end
        end
      end
    end

    describe "list-client-keys" do
      before(:each) do
        allow_any_instance_of(KeyCtlHelper).to receive(:get_rest).with("/organizations/testorg/clients/testclient/keys").and_return(
          [
            {"uri"=>"https://127.0.0.1/organizations/testorg/clients/testclient/keys/default",
             "name"=>"default",
             "expired"=>false},
            {"uri"=>"https://127.0.0.1/organizations/testorg/clients/testclient/keys/key2",
             "name"=>"key2",
             "expired"=>true}
          ]
        )

        allow_any_instance_of(Chef::REST).to receive(:get_rest).with("https://127.0.0.1/organizations/testorg/clients/testclient/keys/default").and_return(
          {"name"=>"default", "public_key"=>public_key, "expiration_date"=>"infinity"}
        )
        allow_any_instance_of(Chef::REST).to receive(:get_rest).with("https://127.0.0.1/organizations/testorg/clients/testclient/keys/key2").and_return(
          {"name"=>"default", "public_key"=>public_key, "expiration_date"=>"2012-01-01T00:00:00Z"}
        )
      end

      it_should_behave_like "a key command with option parsing" do
        let(:command)   { "list-client-keys" }
      end

      it_should_behave_like "a key listing command" do
        let(:command)   { "list-client-keys" }
        let(:arguments) { ["testorg", "testclient"] }
      end
      context "when valid arguments are passed to list-user-keys" do
        it "should make an http request to /users/testuser/keys" do
          expect_any_instance_of(KeyCtlHelper).to receive(:get_rest).with("/organizations/testorg/clients/testclient/keys").at_least(:once)
          @helper.run_test_omnibus_command("list-client-keys", ["testorg", "testclient"])
        end
      end
    end
    context "when a mandatory argument is missing to list-client-keys" do
      context "when ORGNAME is missing" do
        it "should fail with the proper error message" do
          mandatory_argument_should_exist "list-client-keys", "ORGNAME", [], 1
        end
      end
      context "when CLIENTNAME is missing" do
        it "should fail with the proper error message" do
          mandatory_argument_should_exist "list-client-keys", "CLIENTNAME", ["testorg"], 2
        end
      end
    end
  end #listing key commands

  describe "adding key commands" do

    shared_examples_for "a key adding command" do
      it "should POST a proper body to /users/testuser/keys" do
        expect_any_instance_of(KeyCtlHelper).to receive(:post_rest).with(url,
          {
            'name' => "testkey",
            'public_key' => File.read(public_key_path),
            'expiration_date' => "infinity"
          }
        ).at_least(:once)
        @helper.run_test_omnibus_command(command, arguments)
      end
      context "when --key-name isn't passed" do
        context "when PUBLIC_KEY_PATH points to a valid public key" do
          it "should use the fingerprint as the name of the key" do
            expect_any_instance_of(KeyCtlHelper).to receive(:post_rest).with(url,
              {
                'name' => public_key_fingerprint,
                'public_key' => File.read(public_key_path),
                'expiration_date' => "infinity"
              }
            ).at_least(:once)
            @helper.run_test_omnibus_command(command, arguments_no_keyname)
          end
        end
        context "when PUBLIC_KEY_PATH points to a key with a valid header and an invalid body" do
          it "should exit_failure and print fingerprint error message" do
            expect { @helper.run_test_omnibus_command(command, base_arguments.concat([bad_public_key_path])) }.to raise_error(SystemExit, KeyCtlHelper.new.cannot_generate_fingerprint_msg)
          end
        end
        context "when PUBLIC_KEY_PATH points to a key with an invalid header" do
          it "should exit_failure and print the proper message" do
            expect { @helper.run_test_omnibus_command(command, base_arguments.concat([not_a_key_path])) }.to raise_error(SystemExit, KeyCtlHelper.new.not_a_public_key_msg)
          end
        end
        context "when PUBLIC_KEY_PATH points to a file that doesn't exist" do
          it "should exit_failure and print the proper message" do
            expect { @helper.run_test_omnibus_command(command, base_arguments.concat(["/dev/null/not_there"])) }.to raise_error(SystemExit, KeyCtlHelper.new.public_key_path_msg)
          end
        end
      end
    end

    describe "add-user-key" do
      before(:each) do
        allow_any_instance_of(KeyCtlHelper).to receive(:post_rest)
      end

      it_should_behave_like "a key command with option parsing" do
        let(:command)   { "add-user-key" }
      end

      it_should_behave_like "a key command with options that require input" do
        let(:command)   { "add-user-key" }
        let(:valid_arg_missing_input) { "--expiration-date" }
      end

      it_should_behave_like "a key command with multiple options that require input" do
        let(:command)   { "add-user-key" }
        let(:arguments) { ["testuser", "-e", "-k"] }
        let(:argument_followed_by_invalid_input) { "--expiration-date" }
      end

      it_should_behave_like "a key adding command" do
        let(:url)               { "/users/testuser/keys" }
        let(:command)           { "add-user-key" }
        let(:base_arguments)    { ["testuser"] }
        let(:arguments)         { ["testuser", public_key_path, "-k", "testkey"] }
        let(:arguments_no_keyname)  { ["testuser", public_key_path] }
      end
    end

    context "when an invalid date is passed" do
      it "should fail with the proper error message" do
        expect {
          @helper.run_test_omnibus_command("add-user-key",["username", public_key_path, "-e", "invalid-date"])
        }.to raise_error(SystemExit, Regexp.new(KeyCtlHelper.new.invalid_date_msg))
      end
    end

    context "when a mandatory argument is missing to add-user-key" do
      context "when USERNAME is missing" do
        it "should fail with the proper error message" do
          mandatory_argument_should_exist "add-user-key", "USERNAME", [], 1
        end
      end
      context "when PUBLIC_KEY_PATH is missing" do
        it "should fail with the proper error message" do
          mandatory_argument_should_exist "add-user-key", "PUBLIC_KEY_PATH", ["testuser"], 2
        end
      end
    end

    describe "add-client-key" do
      before(:each) do
        allow_any_instance_of(KeyCtlHelper).to receive(:post_rest)
      end

      it_should_behave_like "a key command with option parsing" do
        let(:command)   { "add-client-key" }
      end

      it_should_behave_like "a key command with options that require input" do
        let(:command)   { "add-client-key" }
        let(:valid_arg_missing_input) { "--expiration-date" }
      end

      it_should_behave_like "a key command with multiple options that require input" do
        let(:command)   { "add-client-key" }
        let(:arguments) { ["testorg", "testclient", "-e", "-k"] }
        let(:argument_followed_by_invalid_input) { "--expiration-date" }
      end

      it_should_behave_like "a key adding command" do
        let(:url)                  { "/organizations/testorg/clients/testclient/keys" }
        let(:command)              { "add-client-key" }
        let(:base_arguments)       { ["testorg", "testclient"] }
        let(:arguments)            { ["testorg", "testclient", public_key_path, "-k", "testkey"] }
        let(:arguments_no_keyname) { ["testorg", "testclient", public_key_path] }
      end
    end

    context "when an invalid date is passed" do
      it "should fail with the proper error message" do
        expect {
          @helper.run_test_omnibus_command("add-client-key",["testclient", "testorg", public_key_path,"-e", "invalid-date"])
        }.to raise_error(SystemExit, Regexp.new(KeyCtlHelper.new.invalid_date_msg))
      end
    end

    context "when a mandatory argument is missing to add-client-key" do
      context "when ORGNAME is missing" do
        it "should fail with the proper error message" do
          mandatory_argument_should_exist "add-client-key", "ORGNAME", [], 1
        end
      end
      context "when CLIENTNAME is missing" do
        it "should fail with the proper error message" do
          mandatory_argument_should_exist "add-client-key", "CLIENTNAME", ["testorg"], 2
        end
      end
      context "when PUBLIC_KEY_PATH is missing" do
        it "should fail with the proper error message" do
          mandatory_argument_should_exist "add-client-key", "PUBLIC_KEY_PATH", ["testorg", "testclient"], 3
        end
      end
    end
  end #adding key commands

  describe "deleting key commands" do

    shared_examples_for "a key deleting command" do
      it "should POST a proper body to /users/testuser/keys" do
        expect_any_instance_of(KeyCtlHelper).to receive(:delete_rest).with(url).at_least(:once)
        @helper.run_test_omnibus_command(command, arguments)
      end
    end

    describe "delete-user-key" do
      before(:each) do
        allow_any_instance_of(KeyCtlHelper).to receive(:delete_rest)
      end

      it_should_behave_like "a key deleting command" do
        let(:url)       { "/users/testuser/keys/testkey" }
        let(:command)   { "delete-user-key" }
        let(:arguments) { ["testuser", "testkey"] }
      end

      context "when a mandatory argument is missing to delete-user-key" do
        context "when USERNAME is missing" do
          it "should fail with the proper error message" do
            mandatory_argument_should_exist "delete-user-key", "USERNAME", [], 1
          end
        end
        context "when KEYNAME is missing" do
          it "should fail with the proper error message" do
            mandatory_argument_should_exist "delete-user-key", "KEYNAME", ["testuser"], 2
          end
        end
      end
    end

    describe "delete-client-key" do
      before(:each) do
        allow_any_instance_of(KeyCtlHelper).to receive(:delete_rest)
      end

      it_should_behave_like "a key deleting command" do
        let(:url)       { "/organizations/testorg/clients/testclient/keys/testkey" }
        let(:command)   { "delete-client-key" }
        let(:arguments) { ["testorg", "testclient", "testkey"] }
      end

      context "when a mandatory argument is missing to delete-client-key" do
        context "when ORGNAME is missing" do
          it "should fail with the proper error message" do
            mandatory_argument_should_exist "delete-client-key", "ORGNAME", [], 1
          end
        end
        context "when CLIENTNAME is missing" do
          it "should fail with the proper error message" do
            mandatory_argument_should_exist "delete-client-key", "CLIENTNAME", ["testorg"], 2
          end
        end
        context "when KEYNAME is missing" do
          it "should fail with the proper error message" do
            mandatory_argument_should_exist "delete-client-key", "KEYNAME", ["testorg", "testclient"], 3
          end
        end
      end
    end
  end #deleting key commands
end

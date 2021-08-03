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
require "chef_server_ctl/helpers/key_ctl_helper"
require "chef_server_ctl/config"
require "chef/key"

describe "chef-server-ctl *key(s)*" do
  before(:all) do
    @helper = OmnibusCtlHelper.new(["./plugins/key_control.rb"])
  end

  # marco for testing mandatory args
  def mandatory_argument_should_exist(command, arg_name, arg_list, arg_number)
    expect { @helper.run_test_omnibus_command(command, arg_list) }
      .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
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
    allow(::ChefServerCtl::Config).to receive(:knife_config_file).and_return("#{__dir__}/fixtures/pivotal.rb")
  end

  shared_examples_for "a key command with option parsing" do
    context "when an invalid argument is passed" do
      it "should return a proper error" do
        expect { @helper.run_test_omnibus_command(command, ["--wrong"]) }
          .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
      end
    end
  end

  shared_examples_for "a key command with options that require input" do
    context "when a valid argument is missing valid input" do
      it "should return a proper error" do
        expect { @helper.run_test_omnibus_command(command, [valid_arg_missing_input]) }
          .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
      end
    end
  end

  shared_examples_for "a key command with multiple options that require input" do
    context "when a command that requires input is passed another command" do
      it "should return a proper error" do
        expect { @helper.run_test_omnibus_command(command, arguments) }
          .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
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
      context "when it is called with --verbose" do
        it "should output a public key" do
          expect { @helper.run_test_omnibus_command(command, arguments.concat(["--verbose"])) }.to output(/public_key:\n-----BEGIN PUBLIC KEY-----/).to_stdout
        end
      end
    end

    describe "list-user-keys" do
      before(:each) do
        allow(Chef::Key).to receive(:list_by_user).with("testuser").and_return(
          [
            { "uri" => "https://127.0.0.1/users/pivotal/keys/default",
             "name" => "default",
             "expired" => false },
            { "uri" => "https://127.0.0.1/users/pivotal/keys/key2",
             "name" => "key2",
             "expired" => true },
          ]
        )

        key1 = Chef::Key.new("testuser", "user")
        key1.name "default"
        key1.expiration_date "infinity"
        key1.public_key public_key
        key2 = Chef::Key.new("testuser", "user")
        key2.name "default"
        key2.expiration_date "2012-01-01T00:00:00Z"
        key2.public_key public_key

        allow(Chef::Key).to receive(:list_by_user).with("testuser", inflate = true).and_return([["default", key1], ["key2", key2]])
      end

      it_should_behave_like "a key command with option parsing" do
        let(:command)   { "list-user-keys" }
      end

      it_should_behave_like "a key listing command" do
        let(:command)   { "list-user-keys" }
        let(:arguments) { ["testuser"] }
      end

      context "when valid arguments are passed to list-user-keys" do
        it "should pass the username to Chef::Key.list_by_user" do
          expect(Chef::Key).to receive(:list_by_user).with("testuser").at_least(:once)
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
      context "when --verbose is passed" do
        it "should make a request to Chef::Key.list_by_user with inflate enabled" do
          expect(Chef::Key).to receive(:list_by_user).with("testuser", inflate = true).at_least(:once)
          @helper.run_test_omnibus_command("list-user-keys", ["testuser", "--verbose"])
        end
      end
    end

    describe "list-client-keys" do
      before(:each) do
        allow(Chef::Key).to receive(:list_by_client).with("testclient").and_return(
          [
            { "uri" => "https://127.0.0.1/organizations/testorg/clients/testclient/keys/default",
             "name" => "default",
             "expired" => false },
            { "uri" => "https://127.0.0.1/organizations/testorg/clients/testclient/keys/key2",
             "name" => "key2",
             "expired" => true },
          ]
        )

        key1 = Chef::Key.new("testclient", "client")
        key1.name "default"
        key1.expiration_date "infinity"
        key1.public_key public_key
        key2 = Chef::Key.new("testclient", "client")
        key2.name "default"
        key2.expiration_date "2012-01-01T00:00:00Z"
        key2.public_key public_key

        allow(Chef::Key).to receive(:list_by_client).with("testclient", inflate = true).and_return([["default", key1], ["key2", key2]])
      end

      it_should_behave_like "a key command with option parsing" do
        let(:command)   { "list-client-keys" }
      end

      it_should_behave_like "a key listing command" do
        let(:command)   { "list-client-keys" }
        let(:arguments) { %w{testorg testclient} }
      end
      context "when valid arguments are passed to list-clinet-keys" do
        it "should pass the client to Chef::Key.list_by_client" do
          expect(Chef::Key).to receive(:list_by_client).with("testclient").at_least(:once)
          @helper.run_test_omnibus_command("list-client-keys", %w{testorg testclient})
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
  end # listing key commands

  describe "adding key commands" do

    shared_examples_for "a key adding command" do
      before do
        new_key = Chef::Key.new(actor_name, type)
        new_key.private_key "private_key_data"
        allow_any_instance_of(Chef::Key).to receive(:create).and_return(new_key)
      end
      it "should generate a chef key" do
        expect_any_instance_of(ChefServerCtl::Helpers::KeyCtlHelper).to receive(populate_key_command).with(actor_name, "testkey", File.read(public_key_path), "infinity").at_least(:once).and_return(Chef::Key.new("no_matter", "user"))
        @helper.run_test_omnibus_command(command, arguments)
      end
      it "should call Chef::Key.create" do
        expect_any_instance_of(Chef::Key).to receive(:create).at_least(:once)
        @helper.run_test_omnibus_command(command, arguments)
      end
      context "when --key-name isn't passed" do
        context "when --public-key-path points to a key with an invalid header" do
          it "should exit_failure and print the proper message" do
            expect { @helper.run_test_omnibus_command(command, base_arguments.concat(["-p", not_a_key_path])) }
              .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
          end
        end
        context "when --public-key-path points to a file that doesn't exist" do
          it "should exit_failure and print the proper message" do
            expect { @helper.run_test_omnibus_command(command, base_arguments.concat(["-p", "/dev/null/not_there"])) }
              .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
          end
        end
        context "when --public-key-path is not passed" do
          it "should exit_failure and print the proper message" do
            expect { @helper.run_test_omnibus_command(command, base_arguments) }
              .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
          end
        end
      end
      context "when --public-key-path isn't passed and --key-name is passed" do
        it "should create a new public key and print the private key" do
          expect_any_instance_of(Chef::Key).to receive(:create).at_least(:once)
          expect_any_instance_of(ChefServerCtl::Helpers::KeyCtlHelper).to receive(:print_private_key).at_least(:once)
          @helper.run_test_omnibus_command(command, base_arguments.concat(["-k", "testkey"]))
        end
      end
    end

    describe "add-user-key" do
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
        let(:url)                   { "/users/testuser/keys" }
        let(:command)               { "add-user-key" }
        let(:base_arguments)        { ["testuser"] }
        let(:arguments)             { ["testuser", "-p", public_key_path, "-k", "testkey"] }
        let(:arguments_no_keyname)  { ["testuser", "-p", public_key_path] }
        let(:populate_key_command)  { :populate_user_key }
        let(:actor_name)            { "testuser" }
        let(:type)                  { "user" }
      end
    end

    context "when an invalid date is passed" do
      it "should fail with the proper error message" do
        expect do
          @helper.run_test_omnibus_command("add-user-key", ["username", "-p", public_key_path, "-e", "invalid-date"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
      end
    end

    context "when a mandatory argument is missing to add-user-key" do
      context "when USERNAME is missing" do
        it "should fail with the proper error message" do
          mandatory_argument_should_exist "add-user-key", "USERNAME", [], 1
        end
      end
    end

    describe "add-client-key" do
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
        let(:url)                   { "/organizations/testorg/clients/testclient/keys" }
        let(:command)               { "add-client-key" }
        let(:base_arguments)        { %w{testorg testclient} }
        let(:arguments)             { ["testorg", "testclient", "-p", public_key_path, "-k", "testkey"] }
        let(:arguments_no_keyname)  { ["testorg", "testclient", "-p", public_key_path] }
        let(:populate_key_command)  { :populate_client_key }
        let(:actor_name)            { "testclient" }
        let(:type)                  { "user" }
      end
    end

    context "when an invalid date is passed" do
      it "should fail with the proper error message" do
        expect do
          @helper.run_test_omnibus_command("add-client-key", ["testclient", "testorg", "-p", public_key_path, "-e", "invalid-date"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
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
    end
  end # adding key commands

  describe "deleting key commands" do

    shared_examples_for "a key deleting command" do
      it "should call key.destroy" do
        expect_any_instance_of(Chef::Key).to receive(:destroy).at_least(:once)
        @helper.run_test_omnibus_command(command, arguments)
      end
    end

    describe "delete-user-key" do
      before(:each) do
        allow_any_instance_of(Chef::Key).to receive(:destroy)
      end

      it_should_behave_like "a key deleting command" do
        let(:url)       { "/users/testuser/keys/testkey" }
        let(:command)   { "delete-user-key" }
        let(:arguments) { %w{testuser testkey} }
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
        allow_any_instance_of(Chef::Key).to receive(:destroy)
      end

      it_should_behave_like "a key deleting command" do
        let(:url)       { "/organizations/testorg/clients/testclient/keys/testkey" }
        let(:command)   { "delete-client-key" }
        let(:arguments) { %w{testorg testclient testkey} }
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
            mandatory_argument_should_exist "delete-client-key", "KEYNAME", %w{testorg testclient}, 3
          end
        end
      end
    end
  end # deleting key commands
end

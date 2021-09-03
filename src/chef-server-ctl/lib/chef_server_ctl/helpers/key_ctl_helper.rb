require "chef/config"
require "chef/key"

module ChefServerCtl
  module Helpers
    class KeyCtlHelper
      def initialize
        Chef::Config.from_file(ChefServerCtl::Config.knife_config_file)
      end

      # Optparse doesn't properly handle the case where you specify an argument with mandatory input
      # and then pass another argument after it. It detects the second argument as input to the first argument.
      #
      # For example, if you have two arguments with mandatory input, --username NAME and
      # --public-key-path PATH, defined in optparse, and you pass:
      #
      # cmd --username --public-key-path /etc/key
      #
      # It will detect the input to --username as --public-key-path and won't even detect --public-key-path as an argument.
      # This helper method takes in a list of argument commands and checks that the input to a given argument isn't actually
      # another argument, and throws an error with a relevant message if so, fixing the case that an argument with mandatory
      # input isn't actually eating up the next argument as its input.
      def catch_argument_passed_as_input(arg_list, arg, arg_input)
        arg_list.each do |arg_element|
          if arg_input.strip == arg_element
            exit_failure(missing_input_msg(arg))
          end
        end
      end

      def exit_failure(msg)
        STDERR.puts msg
        raise SystemExit.new(1, msg)
      end

      def populate_client_key(clientname, name, public_key, expiration_date)
        key = Chef::Key.new(clientname, "client")
        populate_key_helper(key, name, public_key, expiration_date)
      end

      def populate_user_key(username, name, public_key, expiration_date)
        key = Chef::Key.new(username, "user")
        populate_key_helper(key, name, public_key, expiration_date)
      end

      def populate_key_helper(key, name, public_key, expiration_date)
        key.name name
        key.expiration_date expiration_date
        if public_key
          key.public_key public_key
        else
          key.create_key true
        end
        key
      end

      def check_valid_iso_date(expiration_date)
        unless /^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z|infinity)$/.match(expiration_date)
          exit_failure(invalid_date_msg)
        end
      end

      def read_and_check_key(key_path)
        begin
          key = File.read(key_path)
        rescue
          exit_failure(public_key_path_msg)
        end

        unless /BEGIN (RSA |)PUBLIC KEY/.match(key)
          exit_failure(not_a_public_key_msg)
        end
        key
      end

      def add_key_usage
        <<EOS

Usage: If --public-key-path isn't passed, the server will generate a public key for you.
Usage: Expiration date defaults to infinity. Pass an ISO 8601 fomatted string: YYYY-MM-DDTHH:MM:SSZ e.g. 2013-12-24T21:00:00Z in UTC timezone.
Usage: Default name used is the fingerprint of the key passed.
EOS
      end

      def parse_missing_arg_error(err)
        arg = err.message.match(/(?<=missing argument:\s).*/)[0]
        exit_failure(missing_valid_input_msg(arg))
      end

      def parse_invalid_arg_error(err)
        arg = err.message.match(/(?<=invalid option:\s).*/)[0]
        exit_failure(invalid_arg_msg(arg))
      end

      def exit_http_fail(err)
        exit_failure("Error: An unexpected error has occured (the server returned a #{err.response.code}).\nError: Please contact a system admin if the problem persists.")
      end

      def get_required_arg!(options, args, usage, field_symbol, field_name, field_number)
        field_value = nil
        if args.nil? || args[field_number - 1].nil?
          exit_failure(usage + argument_missing_msg(field_name, field_number))
        else
          field_value = args[field_number - 1]
        end
        options[field_symbol] = field_value
      end

      def build_key_object(name, key, expiration_date)
        {
          "name" => name,
          "public_key" => key,
          "expiration_date" => expiration_date,
        }
      end

      def output_simple_key_results(results)
        results.each do |result|
          puts "\nname: #{result["name"]}"
          puts "expired: #{result["expired"]}"
        end
      end

      def output_full_key_results(results)
        results.each do |result|
          result = result[1]
          puts "\nname: #{result.name}"
          puts "expiration_date: #{result.expiration_date}"
          puts "public_key:"
          puts result.public_key
        end
      end

      ######################
      # message generators #
      ######################
      def missing_input_msg(arg)
        "Error: Missing valid input for argument #{arg}."
      end

      def invalid_date_msg
        <<EOS
--expiration-date must be followed by a valid ISO 8601 fomatted string YYYY-MM-DDTHH:MM:SSZ e.g. 2013-12-24T21:00:00Z or infinity.
It defaults to infinity if you do not pass --expiration-date.
EOS
      end

      def not_a_public_key_msg
        <<EOS
Error: Invalid public key passed. Key must begin with:
Error: -----BEGIN PUBLIC KEY----- or
Error: -----BEGIN RSA PUBLIC KEY-----
EOS
      end

      def public_key_path_msg
        "Error: --public-key-path PUBLIC_KEY_PATH must be a valid path."
      end

      def missing_valid_input_msg(arg)
        "Error: Missing valid input for argument #{arg}."
      end

      def invalid_arg_msg(arg)
        "Error: Invalid argument #{arg} detected. Please remove or use a different command."
      end

      def argument_missing_msg(field_name, field_number)
        "\nError: You forgot to pass #{field_name}, which should have been argument number #{field_number}."
      end

      def pass_key_name_if_public_key_missing
        <<EOS
Error: You did not pass --public-key-path or --key-name.
Error: A key-name cannot be auto-generated if you do not pass --public-key-path.
Error: Either pass a valid public key via --public-key path or supply a name via --key-name.
EOS
      end

      def print_private_key(key_name, private_key_string)
        puts "New private key for key named #{key_name} (please store, it is not saved in the database):"
        puts private_key_string
      end

    end
  end
end

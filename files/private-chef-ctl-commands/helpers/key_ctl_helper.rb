require "chef/config"
require "chef/rest"

class KeyCtlHelper
  def initialize
    @chef_rest = configure_chef_rest
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

  def get_rest(url)
    @chef_rest.get_rest(url)
  end

  def delete_rest(url)
    @chef_rest.delete_rest(url)
  end

  def post_rest(url, body)
    @chef_rest.post_rest
  end

  def pivotal_config
    "/etc/opscode/pivotal.rb"
  end

  def configure_chef_rest
    Chef::Config.from_file(pivotal_config)
    Chef::REST.new(Chef::Config[:chef_server_root])
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

  def generate_fingerprint(key)
    begin
      openssl_key_object = OpenSSL::PKey::RSA.new(key)
      data_string = OpenSSL::ASN1::Sequence([
                                              OpenSSL::ASN1::Integer.new(openssl_key_object.public_key.n),
                                              OpenSSL::ASN1::Integer.new(openssl_key_object.public_key.e)
                                            ])
      OpenSSL::Digest::SHA1.hexdigest(data_string.to_der).scan(/../).join(':')
    rescue
      exit_failure(cannot_generate_fingerprint_msg)
    end
  end

  def add_key_usage
    "\nUsage: Expiration date defaults to infinity. Pass an ISO 8601 fomatted string: YYYY-MM-DDTHH:MM:SSZ e.g. 2013-12-24T21:00:00Z in UTC timezone\nUsage: Default name used is the fingerprint of the key passed."
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
      'name' => name,
      'public_key' => key,
      'expiration_date' => expiration_date
    }
  end

  def output_key_results(results, show_public_keys)
    results.each do |result|
      puts "\nname: #{result['name']}"
      puts "expired: #{result['expired']}"
      if show_public_keys
        public_key = @chef_rest.get_rest(result["uri"])
        puts "public_key:"
        puts public_key["public_key"]
      end
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

  def cannot_generate_fingerprint_msg
<<EOS
Error: Could not parse fingerprint for public key you passed.
Error: If optional --key-name arg is not specified, then fingerprint of key is used by default as the name.
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
    "Error: PUBLIC_KEY_PATH must be a valid path."
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

end

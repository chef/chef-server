require "chef_server_ctl/log"

# ChefServerCtl::Config is a global configuration class for
# ChefServerCtl subcommands.
#
# We use a global at the moment to avoid too much upheaval in the
# various subcommands.
#
# Configuration is based on environment variables to make it easy to
# implement wrappers in our Habitat packaged versions of chef-server.
#
# If the environment variables become too unwieldy, we can change them
# as long as we remember to go fix the Habitat wrappers.
#
# TODO(ssd) 2018-08-08: Maybe use a configuration file instead?  I've
# opted against it for now to avoid having to write yet-another-file
# out during reconfiugration.
module ChefServerCtl
  module Config
    DEFAULT_KNIFE_CONFIG_FILE = "/etc/opscode/pivotal.rb".freeze
    DEFAULT_KNIFE_BIN = "/opt/opscode/embedded/bin/knife".freeze
    DEFAULT_LB_URL = "https://127.0.0.1".freeze
    DEFAULT_FIPS_LB_URL = "https://127.0.0.1".freeze
    DEFAULT_ERCHEF_REINDEX_SCRIPT = "/opt/opscode/embedded/service/opscode-erchef/bin/reindex-opc-organization".freeze
    DOC_PATENT_MSG = <<-DOC.freeze

Documentation: https://docs.chef.io/server/
Patents:       https://www.chef.io/patents

    DOC

    def self.init(ctl)
      @@ctl = ctl
      Log.debug("Using KNIFE_CONFIG_FILE=#{knife_config_file}")
      Log.debug("Using KNIFE_BIN=#{knife_bin}")
      Log.debug("Using ERCHEF_REINDEX_SCRIPT=#{erchef_reindex_script}")
      Log.debug("Using HABITAT_MODE=#{habitat_mode}")
      # We don't always get run with a full chef-server-running.json
      # so any setting that fallsback to running_config or
      # credentials we can't print here. :(
      # Log.debug("Using BIFROST_URL=#{self.bifrost_url}")
      # Log.debug("Using LB_URL=#{self.lb_url}")
    end

    # knife_config should be the path to a configuration file that
    # allows the `knife` executable to run with pivotal permissions.
    def self.knife_config_file
      ENV["CSC_KNIFE_CONFIG_FILE"] || DEFAULT_KNIFE_CONFIG_FILE
    end

    # knife_bin is the command used to execute knife.
    def self.knife_bin
      ENV["CSC_KNIFE_BIN"] || DEFAULT_KNIFE_BIN
    end

    # fips_enabled indicates whether the chef-server is running in
    # fips mode.
    def self.fips_enabled
      if ENV["CSC_FIPS_ENABLED"]
        ENV["CSC_FIPS_ENABLED"] == "true"
      else
        @@ctl.running_config["private_chef"]["fips_enabled"]
      end
    end

    # The lb_url should be an HTTP address that supports the Chef
    # Server API.
    def self.lb_url
      if ENV["CSC_LB_URL"]
        ENV["CSC_LB_URL"]
      elsif fips_enabled
        DEFAULT_FIPS_LB_URL
      else
        DEFAULT_LB_URL
      end
    end

    # The bifrost_superuser_id is a shared secret of the bifrost
    # service that allows us to make requests without access controls.
    def self.bifrost_superuser_id
      @@bifrost_superuser_id ||= ENV["CSC_BIFROST_SUPERUSER_ID"] || @@ctl.credentials.get("oc_bifrost", "superuser_id")
    end

    # bifrost_url is an HTTP url for the Bifrost authentication
    # service.
    def self.bifrost_url
      @@bifrost_url ||= if ENV["CSC_BIFROST_URL"]
                          ENV["CSC_BIFROST_URL"]
                        else
                          bifrost_config = @@ctl.running_service_config("oc_bifrost")
                          vip = bifrost_config["vip"]
                          port = bifrost_config["port"]
                          "http://#{vip}:#{port}"
                        end
    end

    # bifrost_sql_connuri returns a string in the libpq connection URI
    # format. This string is suitable for passing directly to
    # ::PGConn.open.
    #
    # We connect to bifrost as the superuser since we need to create
    # tables in cleanup-bfirost
    def self.bifrost_sql_connuri
      @@bifrost_connuri ||= if ENV["CSC_BIFROST_DB_URI"]
                              ENV["CSC_BIFROST_DB_URI"]
                            else
                              pg_config = @@ctl.running_service_config("postgresql")
                              user = pg_config["db_connection_superuser"] || pg_config["db_superuser"]
                              password = @@ctl.credentials.get("postgresql", "db_superuser_password")
                              make_connection_string("bifrost", user, password)
                            end
    end

    # erchef_sql_connuri returns a string in the libpq connection URI
    # format. This string is suitable for passing directly to
    # ::PGConn.open.
    def self.erchef_sql_connuri
      @@erchef_connuri ||= if ENV["CSC_ERCHEF_DB_URI"]
                             ENV["CSC_ERCHEF_DB_URI"]
                           else
                             erchef_config = @@ctl.running_service_config("opscode-erchef")
                             user = erchef_config["sql_connection_user"] || erchef_config["sql_user"]
                             password = @@ctl.credentials.get("opscode_erchef", "sql_password")
                             make_connection_string("opscode_chef", user, password)
                           end
    end

    # erchef_reindex_script is a command to execute to run the erchef
    # reindex RPC calls. This is an RPC script that is part of the
    # erchef application.
    def self.erchef_reindex_script
      ENV["CSC_ERCHEF_REINDEX_SCRIPT"] || DEFAULT_ERCHEF_REINDEX_SCRIPT
    end

    # habitat_mode is a boolean that is true if running in habitat
    # mode.
    def self.habitat_mode
      ENV["CSC_HABITAT_MODE"] == "true"
    end

    def self.make_connection_string(db_name, db_user, db_password)
      pg_config = @@ctl.running_service_config("postgresql")
      host = pg_config["vip"]
      port = pg_config["port"]
      "postgresql:///#{db_name}?user=#{db_user}&password=#{db_password}&host=#{host}&port=#{port}"
    end

    def self.ssl_params
      return {} unless habitat_mode

      # net/http doesn't allow CN customization so we can't mutually verify
      params = { verify_ssl: OpenSSL::SSL::VERIFY_NONE }
      if ENV.key?("CSC_TLS_CRT")
        crt =
          if File.exist?(ENV["CSC_TLS_CRT"])
            File.read(ENV["CSC_TLS_CRT"])
          else
            ENV["CSC_TLS_CRT"]
          end

        params[:ssl_client_cert] = OpenSSL::X509::Certificate.new(crt)
      end

      if ENV.key?("CSC_TLS_KEY")
        key =
          if File.exist?(ENV["CSC_TLS_KEY"])
            File.read(ENV["CSC_TLS_KEY"])
          else
            ENV["CSC_TLS_KEY"]
          end

        params[:ssl_client_key] = OpenSSL::PKey::RSA.new(key)
      end

      if ENV.key?("CSC_TLS_CA")
        params[:ssl_ca_file] = ENV["CSC_TLS_CA"]
      end

      params
    end
  end
end

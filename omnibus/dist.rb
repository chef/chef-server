class Chef
  class Dist
    class Server
      # This class is not fully implemented, depending on it is not recommended!
      # When referencing a product directly, like Chef (Now Chef Infra)
      PRODUCT = "Chef Infra Server".freeze

      # A short designation for the product, used in Windows event logs
      # and some nomenclature.
      SHORT = "chef-server".freeze

      # product website address
      WEBSITE = "https://github.com/chef/chef-server".freeze

      # The configuration directory
      CONF_DIR = "/etc/chef-server".freeze

      # The server's configuration utility
      CTL = "chef-server-ctl".freeze
    end
  end
end
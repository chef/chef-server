require 'chef/provider/lwrp_base'

class Chef
  class Provider
    class OcIdApplication < Chef::Provider::LWRPBase
      include Chef::Mixin::ShellOut

      use_inline_resources if defined?(:use_inlined_resources)

      action :create do
        converge_by "create oc-id application '#{new_resource.name}'" do
          attributes = create!

          directory '/etc/opscode/oc-id-applications' do
            owner 'root'
            group 'root'
            mode '0755'
          end

          file "/etc/opscode/oc-id-applications/#{new_resource.name}.json" do
            content Chef::JSONCompat.to_json_pretty(attributes)
            owner 'root'
            group 'root'
            mode '0600'
          end
        end
      end

      private

      def create!
        @attributes ||= Chef::JSONCompat.from_json(command("
          app = Doorkeeper::Application.find_or_create_by(\
            :name => \"#{new_resource.name}\");\
          app.update_attributes(\
            :redirect_uri => \"#{new_resource.redirect_uri}\");\
          puts app.to_json
          ")).delete_if { |key| %w[ id created_at updated_at].include? key }
      end

      def command(text)
        shell_out!("bin/rails runner -e production '#{text}'",
                   :cwd => '/opt/opscode/embedded/service/oc_id').stdout.chomp
      end
    end
  end
end

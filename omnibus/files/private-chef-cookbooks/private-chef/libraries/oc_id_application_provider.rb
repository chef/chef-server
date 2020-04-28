require 'chef/provider/lwrp_base'

class Chef
  class Provider
    class OcIdApplication < Chef::Provider::LWRPBase
      include Chef::Mixin::ShellOut

      provides :oc_id_application

      action :create do
        converge_by "create oc-id application '#{new_resource.name}'" do
          attributes = create!

          if new_resource.write_to_disk
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
      end

      private

      def create!
        @attributes ||= begin
                          env_helper = "veil-env-helper --use-file -s #{Chef::Dist::Server::SHORT}.webui_key -s oc_id.sql_password -s oc_id.secret_key_base"
                          rails_script = <<~EOF
                            app = Doorkeeper::Application.find_or_create_by(:name => "#{new_resource.name}");
                            app.update_attributes(:redirect_uri => "#{new_resource.redirect_uri}");
                            puts app.to_json
                          EOF
                          # in order to account for rails logging, we take only the last line of output
                          # from the rails runner script. if the logging is parsed as json, we end up
                          # with a difficult-to-comprehend error message that looks like:
                          #
                          # ```
                          # Chef::Exceptions::JSON::ParseError: lexical error: invalid char in json text.
                          #                            I, [2015-05-07T18:26:37.236655
                          #          (right here) ------^
                          # ```
                          json = shell_out!("#{env_helper} -- bin/rails runner -e production '#{rails_script}'",
                            cwd: '/opt/opscode/embedded/service/oc_id').stdout.lines.last.chomp

                          Chef::JSONCompat.from_json(json).delete_if { |key| %w( id created_at updated_at).include? key }
                        end
      end
    end
  end
end

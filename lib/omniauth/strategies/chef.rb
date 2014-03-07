require 'chef'

module OmniAuth
  module Strategies
    class Chef
      include OmniAuth::Strategy

      option :fields, :name
      option :uid,    :name

      def request_phase
        form = OmniAuth::Form.new title: 'Authenticate', url: callback_path

        form.text_field     'Username', 'username'
        form.password_field 'Password', 'password'

        form.button 'Authenticate'

        form.to_response
      end

      def callback_phase
        @user = authenticate request['username'], request['password']

        if authenticated? @user
          uid do
            request['username']
          end

          extra do
            { }
          end
        else
          fail! :invalid_credentials
        end
      end

      def authenticate username, password
        begin
          chef('pivotal').post_rest('authenticate_user', username: username, password: password)
        rescue Net::HTTPServerException
          nil
        end
      end

      def authenticated? user
        user['status'] ? true : false
      end

      def chef user, organization = nil
        ::Chef::REST.new endpoint(organization), user, nil, headers: request_source, raw_key: key
      end

      def endpoint organization = nil
        "https://api.opscode.piab#{organization}"
      end

      def key path = 'config/webui_priv.pem'
        IO.read(File.expand_path "../../../../#{path}", __FILE__).strip
      end

      def request_source
        { 'x-ops-request-source' => 'web' }
      end
    end
  end
end

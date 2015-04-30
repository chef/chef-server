module Pedant
  module ACL
    extend Pedant::Concern
    def restrict_permissions_to(acl_base, permissions = {})
      permissions[superuser] = %w(create read update delete grant) if !permissions[superuser]
      @original_permissions ||= {}
      @original_permissions[acl_base] = get(api_url("#{acl_base}/_acl"), superuser)
      %w(create read update delete grant).each do |type|
        users = permissions.keys.select { |user| permissions[user].include?(type) }.map { |user| user.name }
        response = put(api_url("#{acl_base}/_acl/#{type}"), superuser, :payload => {
          type => {
            'actors' => users,
            'groups' => []
          }
        })
        response.should look_like({ :status => 200 })
      end
    end

    def unrestrict_permissions
      if @original_permissions
        @original_permissions.each do |acl_base, acl|
          acl = parse(acl)
          acl.each do |type, type_acl|
            response = put(api_url("#{acl_base}/_acl/#{type}"), superuser, :payload => {
              type => type_acl
            })
            # 404 is OK, the test may have deleted the thing
            if response.code != 404
              response.should look_like({ :status => 200 })
            end
          end
        end
        @original_permissions = nil
      end
    end

    included do
      after(:each) { unrestrict_permissions }
    end

  end
end

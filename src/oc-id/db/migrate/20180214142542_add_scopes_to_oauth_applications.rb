class AddScopesToOauthApplications < ActiveRecord::Migration[6.0]
  def change
    add_column :oauth_applications, :scopes, :string, null: false, default: ''
  end
end

class AddScopesToOauthApplications < ActiveRecord::Migration[7.0]
  def change
    add_column :oauth_applications, :scopes, :string, null: false, default: ''
  end
end

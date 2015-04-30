class ChangeColumnTypeForResourceOwnerId < ActiveRecord::Migration
  def change

    change_table :oauth_access_grants do |t|
      t.change :resource_owner_id, :string
    end

    change_table :oauth_access_tokens do |t|
      t.change :resource_owner_id, :string
    end

  end
end

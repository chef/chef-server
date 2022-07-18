# frozen_string_literal: true

class AddConfidentialToApplications < ActiveRecord::Migration[7.0]
  def change
    add_column(
      :oauth_applications,
      :confidential,
      :boolean,
      null: false,
      default: false
    )
  end
end

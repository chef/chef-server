actions :deploy
default_action :deploy

attribute :name,           kind_of: String, required: true, name_attribute: true
attribute :database,       kind_of: String, required: true
attribute :username,       kind_of: String, required: true
attribute :password,       kind_of: String, required: true
attribute :hostname,       kind_of: String
attribute :port,           kind_of: Integer
attribute :target_version, kind_of: String
attribute :verify,         kind_of: [TrueClass, FalseClass], default: true

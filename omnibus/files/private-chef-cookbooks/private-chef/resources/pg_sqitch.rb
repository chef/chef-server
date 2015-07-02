actions :deploy
default_action :deploy

attribute :name,           kind_of: String, required: true, name_attribute: true
attribute :database,       kind_of: String, required: true
attribute :username,       kind_of: String
attribute :password,       kind_of: String, required: false, default: ""
attribute :target_version, kind_of: String
attribute :hostname,       kind_of: String, required: true
attribute :port,           kind_of: Integer, required: true
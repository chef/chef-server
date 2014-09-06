actions :create

default_action :create

attribute :username,
:kind_of => String,
:name_attribute => true

attribute :database,
:kind_of => String,
:required => true

attribute :schema,
:kind_of => String,
:required => true

attribute :access_profile,
:equal_to => [:write, :read],
:required => true

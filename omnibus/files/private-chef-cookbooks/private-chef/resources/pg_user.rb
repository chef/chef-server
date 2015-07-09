actions :create

default_action :create

attribute :username,
:kind_of => String,
:name_attribute => true

attribute :password,
:kind_of => String,
:required => true

attribute :superuser,
:kind_of => [TrueClass, FalseClass],
:default => false

attribute :local_connection,
:kind_of => [TrueClass, FalseClass],
:default => false

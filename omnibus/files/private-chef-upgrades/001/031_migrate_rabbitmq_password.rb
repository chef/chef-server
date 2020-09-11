define_upgrade do
  # This migration previously attempted to reset the RabbitMQ
  # passwords. Chef Server 14 removed RabbitMQ and this migration now
  # does nothing.
end

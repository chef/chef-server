require 'pp'
require 'stringio'
require 'rubygems'
require 'bunny'
$:.unshift(File.dirname(__FILE__) + '/../lib/')
require 'opscode/expander'

include Opscode

OPSCODE_EXPANDER_MQ_CONFIG = {:user => "guest", :pass => "guest", :vhost => '/opscode-expander-test'}

begin
  b = Bunny.new(OPSCODE_EXPANDER_MQ_CONFIG)
  b.start
  b.stop
rescue Bunny::ProtocolError, Bunny::ServerDownError, Bunny::ConnectionError => e
  STDERR.puts(<<-ERROR)

****************************** FAIL *******************************************
* Running these tests requires a running instance of rabbitmq
* You also must configure a vhost "/opscode-expander-test"
* and a user "guest" with password "guest" with full rights
* to that vhost
-------------------------------------------------------------------------------
> rabbitmq-server
> rabbitmqctl add_vhost /opscode-expander-test
> rabbitmqctl set_permissions -p /opscode-expander-test guest '.*' '.*' '.*'
> rabbitmqctl list_user_permissions guest
****************************** FAIL *******************************************

ERROR
  if ENV['DEBUG'] == "true"
    STDERR.puts("#{e.class.name}: #{e.message}")
    STDERR.puts("#{e.backtrace.join("\n")}")
  end
  exit(1)
end


# OPSCODE EXPANDER CONFIGURATION ######
# A Sample config file for spec tests #
#######################################

## The Actual Config Settings for Opscode Expander ##
# Solr
solr_url        "http://localhost:8983"

## Parameters for connecting to RabbitMQ ##
# Defaults:
#amqp_host   'localhost'
#amqp_port   '5672'
#amqp_user   'guest'
amqp_pass   'config-file' # should override the defaults
amqp_vhost  '/config-file'

## Cluster Config, should be overridden by command line ##
node_count 42
## Extraneous Crap (should be ignored and not raise an error) ##

solr_ram_use "1024T"
another_setting "#{solr_ram_use} is an alot"
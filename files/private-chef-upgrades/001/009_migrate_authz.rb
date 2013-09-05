# Switching from authz to bifrost.

define_upgrade do

  # Shut down everything but couch & postgres
  run_command("private-chef-ctl stop nginx")
  run_command("private-chef-ctl stop opscode-org-creator")
  run_command("private-chef-ctl stop bookshelf")
  run_command("private-chef-ctl stop oc_bifrost")
  run_command("private-chef-ctl stop opscode-account")
  run_command("private-chef-ctl stop opscode-certificate")
  run_command("private-chef-ctl stop opscode-erchef")
  run_command("private-chef-ctl stop opscode-expander")
  run_command("private-chef-ctl stop opscode-expander-reindexer")
  run_command("private-chef-ctl stop opscode-solr")
  run_command("private-chef-ctl stop opscode-webui")
  run_command("private-chef-ctl stop opscode-rabbitmq")

  migrate_script = "/opt/opscode/embedded/service/oc_authz_migrator/scripts/opc-run.sh"
  run_command(migrate_script)

  # Bring everything back up
  run_command("private-chef-ctl start opscode-rabbitmq")
  run_command("private-chef-ctl start opscode-webui")
  run_command("private-chef-ctl start opscode-solr")
  run_command("private-chef-ctl start opscode-expander-reindexer")
  run_command("private-chef-ctl start opscode-expander")
  run_command("private-chef-ctl start opscode-erchef")
  run_command("private-chef-ctl start opscode-certificate")
  run_command("private-chef-ctl start opscode-account")
  run_command("private-chef-ctl start oc_bifrost")
  run_command("private-chef-ctl start bookshelf")
  run_command("private-chef-ctl start opscode-org-creator")
  run_command("private-chef-ctl start nginx")

end

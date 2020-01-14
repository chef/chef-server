# Switching from authz to bifrost.

define_upgrade do

  if Partybus.config.bootstrap_server
    must_be_data_master

    down_services = ['nginx',
                     'opscode-org-creator',
                     'bookshelf',
                     'oc_bifrost',
                     'opscode-account',
                     'opscode-certificate',
                     'opscode-erchef',
                     'opscode-expander',
                     'opscode-expander-reindexer']

    # Shut down everything but couch & postgres
    down_services.each{|s| run_command("private-chef-ctl stop #{s}")}

    migrate_script = "/opt/opscode/embedded/service/oc_authz_migrator/scripts/opc-run.sh"
    run_command("#{migrate_script} #{Partybus.config.couchdb_data_dir}/authorization.couch")

  end

end

# Switching from authz to bifrost.

define_upgrade do
  migrate_script = "/opt/opscode/embedded/service/oc_authz_migrator/scripts/opc-run.sh"
  run_command(migrate_script)
end

This document provides an overview of the the general approach to
performing per-org migrations using darklaunch controls, when the
components being migrated affect pre-created orgs.

# General Migration Flow

1. Migrate select orgs (internal, customer) individually using
   chef-mover tooling.
    1. build updated dets files
    2. using ``moser_state_tracker:insert_one_org`` create migration
       state tracking records for the orgs to be migrated.
    3. perform migration normally using ``mover_manager:migrate``
2. Ensure all newly created orgs use the new component, while forcing
   all existing orgs to use the old component.
    1. shut off org precreation internally. (stop org-creator)
    2. Disable external org creation (darklaunch flag)
    3. Delete precreated orgs (can be done any time before re-enabling
       org creation job, since org capture in chef-mover should exclude
       precreated orgs.
    4. build updated dets files
    5. run mover migration to capture all existing orgs not already
       captured into ``org_migration_state`` and to set appropriate
       redis flag for each to explicitly force the org to use the old
       component.
    6. update ``dl_default`` key in redis with a value to route any orgs
       without a specific value to the new component
    7. Enable org-creator service
    8. Once a sufficient quantity of orgs exist, re-enable customer org
       creation via darklaunch.
3. Unmigrated orgs are a fixed list, and are now captured in
   ``org_migration_state`` in a ``holding`` state - they must be set to
   ``ready`` singly or in groups before ``mover_manager:migrate`` will
    see them.


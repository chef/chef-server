# Copyright (c) 2013 Opscode, Inc.
# All Rights Reserved

add_command_under_category "reindex", "general", "Reindex all server data for a given organization", 2 do
  reindex_args = ARGV[3..-1] # Chop off first 3 args, keep the rest... that is, everything after "private-chef-ctl reindex"
  organization = reindex_args[0]

  # Always perform a complete reindexing; if you want more granular options,
  # use the reindex-opc-server escript directly
  Dir.chdir(File.join(base_path, "embedded", "service", "opscode-erchef", "bin")) do
    exec("./reindex-opc-organization complete #{organization}")
  end
end

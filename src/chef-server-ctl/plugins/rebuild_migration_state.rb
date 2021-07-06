add_command_under_category "rebuild-migration-state", "general", "Attempt to rebuild the migration-state file before upgrade.", 2 do
  require "optparse"
  rebuild_args = ARGV[1..-1]
  options = {}

  OptionParser.new do |opts|
    opts.banner = "chef-server-ctl rebuild-migration-state [options]"
    opts.on("--force", "Overwrite existing migration state.") do |b|
      options[:force] = true
    end
  end.parse!(rebuild_args)

  Dir.chdir(File.join(base_path, "embedded", "service", "partybus"))
  bundle = File.join(base_path, "embedded", "bin", "bundle")
  force_arg = options[:force] ? " force" : ""
  status = run_command("#{bundle} exec ./bin/partybus infer#{force_arg}")
  if status.success?
    exit 0
  else
    exit 1
  end
end

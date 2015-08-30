require "optparse"
DESC = "Reconfigure this server to join an existing cluster.  Provide the FQDN of the cluster or any single node in it."

add_command_under_category "cluster-join", "high-availability", DESC, 2 do
  def scarywarning(myname)
    log <<EOM
  * * * * * * * * * * * * * WARNING * * * * * * * * * * * * * *

   This will join this Chef Server Node to an existing cluster
   and replace all configuration present /etc/opscode.

   This node's FQDN is '#{myname}'.

   Press CTRL+C now if the remote host will not be able to reach this node
   using that name. This node will not be able to join the cluster
   if the remote node can't reach it.

   If there is a different FQDN to use, specify it with the --fqdn option.

  * * * * * * * * * * * * * WARNING * * * * * * * * * * * * * *
EOM
  end

  my_name = `hostname`.chomp
  banner = <<EOM
Usage: #{@exename} cluster-join REMOTE-FQDN [--fqdn THIS-HOST-FQDN] [--help]

REMOTE-FQDN is the name of a remote Chef Server in the cluster. This name should be
resolvable via DNS or hosts file.  Note that if it's a fully qualified name (eg host.example.com)
then the fqdn of this host must also be a fully qualified name.
EOM
  opt_parser = OptionParser.new do |opts|
    opts.banner = banner

    opts.on("-h", "--help", "Shows help.") do
      log opts
      raise SystemExit.new(0, "help")
    end
    opts.on("-f", "--fqdn FQDN", "Fully qualified name of this host, resolvable via hostsfile or dns from the remote Chef Server node") do |name|
      my_name = name
    end
  end
  # Ugly hack because Ctl doens't give us a clean set of args, and other
  # global option (--verbose, -q ) are permitted  - so remove anything we
  # don't want to see.
  cmd_args = ARGV[3..-1].reject { |arg| arg.start_with?("-") && !(arg == "--fqdn" || arg == "-f" ) }
  begin
    opt_parser.parse!(cmd_args)
  rescue OptionParser::MissingArgument => e
    puts e.msg
    raise SystemError(1, e.msg)
  end
  if cmd_args.include? "help"  || cmd_args.length != 2
    # ... we really need to fix base Ctl to allow for "help command-name"
    usage
    raise SystemExit.new(1, "fail")
  end
  puts "***** #{cmd_args}"
  remotenode = cmd_args[0]
  scarywarning(my_name)
  `mkdir -p /etc/opscode`
  `rm -rf /etc/opscode/*`
  run_command "/opt/opscode/embedded/service/omnibus-ctl/node_connector.es #{my_name} #{remotenode}"
  if $?.status == 0
    # Okay! This means we've gotten all of our config, have a proper chef-server.rb -
    # so we'll reconfigure and be done.
    log "Registration complete!"
    log "Reconfiguring chef-server, this may take up to three minutes."
    # Make reconfigure quiet.
    @quiet = true
    reconfigure(true)
    log "This node can now be added to the load balancer rotation."
    log "Happy Cheffing!"

  else
    raise SystemExit.new($?.status, "fail")
  end

end

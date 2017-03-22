module DVM
  class RailsProject < Project
    attr_reader :install_options, :port, :host, :environment, :build_steps, :symlinks
    def initialize(project_name, config)
      super
      @install_options = project['install_options']
      @port = service['port'] || '3000'
      @host = service['host'] || '127.0.0.1'
      @build_steps = project['build_steps'] || []
      @symlinks = project['symlinks'] || []
      @loaded = false
    end

    #
    # Project Commands
    #
    def do_load(options)
      raise DVMArgumentError, "Project already loaded. Use --force to proceed." if loaded? and not options[:force]
      run_command("rm -rf .bundle/config", "Cleanup bundle config", cwd: project_dir)
      run_command("bundle install #{install_options}", "Installing gem dependencies...", cwd: project_dir)
      disable_service
      do_links
      do_build unless options[:no_build]
      @loaded = true
      say(HighLine.color("Success! Your project is loaded.", :green))
      say("Start it now:")
      say(HighLine.color("    dvm start #{name} [--background]", :green))
    end

    def start(args, background)
      raise DVMArgumentError, "#{name} appears to be already running." if is_running?
      if background
        exec "#{server_start_cmd} > /var/log/#{omnibus_project}/#{service['name']}/current 2>&1 &", close_others: false
      else
        exec server_start_cmd, close_others: false
      end
    end

    def unload
      purge_links
      enable_service
      @loaded = false
    end

    def stop
      raise DVMArgumentError, <<-EOM unless is_running?
"#{name} does not seem to be running from a loaded instance.
If you want to stop the global instance, use chef-server-ctl stop #{service['name']}'"
EOM
      run_command("kill -9 $(cat tmp/pids/server.pid)", "Stopping #{service['name']} Rails Server", cwd: project_dir)
    end

    def loaded?
      @loaded
    end

    #
    # Helpers
    #
    def do_links
      symlinks.each do |sync_file, omnibus_file|
        if File.exists?("#{project_dir}/#{sync_file}") && !File.symlink?("#{project_dir}/#{sync_file}")
          raise DVMArgumentError, "The sync utility will overwrite symlinks. Please add #{sync_file} to the exluded-files list."
        end
        from_file = "#{project_dir}/#{sync_file}"
        to_file = "/var/opt/#{omnibus_project}/#{service['name']}/#{omnibus_file}"
        say("Creating Symlink: #{from_file} => #{to_file}")
        FileUtils.ln_sf(to_file, from_file)
      end
    end

    def do_build
      build_steps.each do |step|
        run_command("#{step}", "Build Step: '#{step}'", cwd: project_dir, env: {'RAILS_ENV' => 'production'})
      end
    end

    def purge_links
      symlinks.each do |sync_file, omnibus_file|
        from_file = "#{project_dir}/#{sync_file}"
        to_file = "/var/opt/#{omnibus_project}/#{service['name']}/#{omnibus_file}"
        say("Removing Symlink: #{from_file} => #{to_file}")
        FileUtils.rm_rf(from_file)
      end
    end

    def server_start_cmd
      "cd #{project_dir} && bundle exec --keep-file-descriptors bin/rails server -p #{port} -b #{host} -e production"
    end

    def enable_service
      run_command("chef-server-ctl start #{service['name']}", "Starting #{omnibus_project} #{service['name']}")
    end

    def disable_service
      run_command("chef-server-ctl stop #{service['name']}", "Stopping #{omnibus_project} #{service['name']}")
    end

    def is_running?
      File.exist?(File.join(project_dir, 'tmp/pids/server.pid'))
    end
  end
end

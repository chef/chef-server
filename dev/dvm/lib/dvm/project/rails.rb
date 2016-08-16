module DVM
  class RailsProject < Project
    attr_reader :install_options, :database, :port, :host, :environment
    def initialize(project_name, config)
      super
      @install_options = project['install_options']
      @database = service['database'] || 'config/database.yml'
      @port = service['port'] || '3000'
      @host = service['host'] || '127.0.0.1'
      @environment = service['environment'] || 'production'
    end
    def do_load(options)
      run_command("chef-server-ctl stop #{service['name']}", "Stopping #{omnibus_project} #{service['name']}")
      install
      post_install
      start_server
    end
    def install
      run_command("rm -rf .bundle/config", "Cleanup bundle config", cwd: project_dir)
      run_command("bundle install #{install_options}", "Installing gem dependencies...", cwd: project_dir)
    end
    def post_install
      if has_database?
        run_command("ln -sf /var/opt/#{omnibus_project}/#{service['name']}/#{database} #{File.join(project_dir, database)}", "Symlink database configuration", cwd: project_dir)
      end
      run_command("ln -sf /var/opt/#{omnibus_project}/#{service['name']}/tmp #{File.join(project_dir, 'tmp')}", "Symlink tmp directory", cwd: project_dir)
    end
    def start_server
      say "Starting #{service['name']} Rails Server"
      # We can't trigger a background process via run_command, and -d won't redirect STDOUT
      exec "cd #{project_dir} && bundle exec rails server -p #{port} -b #{host} -e #{environment} > /var/log/#{omnibus_project}/#{service['name']}/current 2>&1 &"

    end
    def unload
      run_command("kill -9 $(cat tmp/pids/server.pid)", "Stopping #{service['name']} Rails Server", cwd: project_dir)
      run_command("chef-server-ctl start #{service['name']}", "Restarting #{omnibus_project} #{service['name']}")
    end
    def loaded?
      path_mounted?(project_dir)
    end
    def has_database?
      File.exist?(File.join(project_dir, database))
    end
  end
end

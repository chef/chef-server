module DVM
  module Tools
    def run_command(command, desc = nil, opts = {})
      no_raise = opts.delete(:no_raise)
      say desc if desc
      cmd = Mixlib::ShellOut.new(command, opts)
      cmd.run_command
      cmd.error! unless no_raise
      cmd
    end

    def bind_mount(from, to)
      if path_mounted?(from)
        puts "#{from} already mounted, taking no action"
        return
      end
      if find_mount(File.read('/etc/fstab'), from)
        puts "Not adding #{from} to /etc/fstab, it already exists"
      else
        # Add an fstab entry so we don't lose this on reboot.
        File.open('/etc/fstab', 'a') { |fstab| fstab.puts "#{from} #{to} none bind" }
      end
      run_command("mount #{from}")
    end

    def unmount(from)
      unless path_mounted?(from)
        puts "#{from} not mounted, taking no action"
        return
      end

      run_command "umount #{from}"
      lines = File.read('/etc/fstab').split("\n")
      File.open('/etc/fstab', 'w') do |fstab|
        # Write everything except the item we're removing.
        lines.each do |line|
          fstab.puts line unless line.include? from
        end
      end
    end

    def find_mount(mount_list, path)
      mount_list.split("\n").each do |line|
        return true if line.include?(path)
      end
      return false
    end

    def path_mounted?(path)
      mounts = run_command "mount"
      find_mount(mounts.stdout, path)
    end

    def system_gem_path(name)
       gem_path = `gem which #{name} 2>&1`
       Pathname.new(gem_path).parent.parent.to_s
    end
    def host_raw_dir
        "/host"
    end

    def host_project_dir(path)
      File.join(host_raw_dir, path)
    end

    def clone(name, uri)
      run_command("git clone '#{uri}' '#{name}'", "Cloning #{name} to host. For future reference, you may also symlink it into chef-server/external-deps from another location on the host.",
                  cwd: host_external_deps_dir)
    end

    def project_dir_exists_on_host?(name)
      puts "Checking #{host_project_dir(name)} "
      File.directory?(host_project_dir(name))
    end

    def checkout(name, ref)
      cwd = host_project_dir(name)
      result = run_command("git checkout #{ref}", "Checking out #{ref} to match what is currently running", cwd: cwd, no_raise: true)
      if result.error?
        raise DVM::DVMArgumentError, <<-EOM
Could not check out #{ref} in #{cwd}.
Have you pulled the latest and/or stashed local changes?
Alternatively, specify '--no-checkout'.

Error was: #{result.stderr}"
EOM
      end
    end
  end
end

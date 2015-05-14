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
      run_command "mount -o bind #{from} #{to}"
    end
    def unmount(from)
      run_command "umount #{from}"
    end

    def path_mounted?(path)
      mounts = run_command "mount"
      mounts.stdout.split("\n").each do |mount|
        if mount.include? path
          return true
        end
      end
      return false
    end

    def host_raw_dir
      "/mnt/host-do-not-use"
    end

    def host_project_dir(path)
      File.join(host_raw_dir, path)
    end

    def clone(name, uri)
      run_command("git clone '#{uri}' '#{name}'", "Cloning #{name} to host.", cwd: host_raw_dir)
    end

    def project_dir_exists_on_host?(name)
      File.directory? host_project_dir(name)
    end

    def checkout(name, ref)
      result = run_command("git checkout #{ref}", "Checking out #{ref} to match what is currently running", cwd: host_project_dir(name), no_raise: true)
      if result.error?
        raise DVM::DVMArgumentError, "Could not check out #{ref} in #{host_project_dir(name)}.  Have you pulled the latest and/or stashed local changes?\nError was: #{result.stderr}"
      end
    end
  end
end

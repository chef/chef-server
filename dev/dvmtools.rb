
def load_settings
  attributes = YAML.load_file("defaults.yml")
  begin
    custom_attributes = YAML.load_file("config.yml")
    attributes = simple_deep_merge(attributes, custom_attributes)
  rescue
  end
  attributes
end


def simple_deep_merge(source_hash, new_hash)
  source_hash.merge(new_hash) do |key, old, new|
    if new.respond_to?(:blank) && new.blank?
      old
    elsif (old.kind_of?(Hash) and new.kind_of?(Hash))
        simple_deep_merge(old, new)
    elsif (old.kind_of?(Array) and new.kind_of?(Array))
        old.concat(new).uniq
    else
       new
    end
  end
end

class PackagePrompt
    attr_accessor :package_name, :package_title, :installer_var, :autopackage_var

    def initialize(package_name, package_title, installer_var, autopackage_var)
        @package_name = package_name
        @package_title = package_title
        @installer_var = installer_var
        @autopackage_var = autopackage_var

    end

    def prepare(action)
        if action =~ /^(provision|up|reload)$/
            installer = prompt_installer
            raise "Please set #{installer_var} to the path of a .deb package for #{package_title}." if installer.nil?
            raise "#{installer} does not exist! Please fix this." unless File.file?(installer)
            installer_path = File.dirname(File.expand_path(installer))
            provisioning = true
        end
        [provisioning, installer, installer_path]
    end

    def prompt_installer
        puts "Looking in #{Dir.home}/Downloads and #{base_path}/omnibus/pkg for installable #{@package_name} package."
        # TODO allow config override of location, multiple locations, search pattern, max count?
        files = Dir.glob("#{Dir.home}/Downloads/#{@package_name}*.deb") + Dir.glob("#{base_path}/omnibus/pkg/#{@package_name}*.deb")

        if ENV[@installer_var]
            if ENV[@installer_var] =~ /^.*#{@package_name}.*deb$/ and File.file?(ENV[@installer_var])
                user_installer = File.expand_path(ENV[@installer_var])
            else
                puts "#{@installer_var} #{ENV[@installer_var]} is not a valid #{@package_name} package. Ignoring."
            end
        end

        if files.length == 0 and not user_installer
            return nil
        end

        files = files.sort_by{ |f| File.mtime(f) }.last(10)
        files.reverse!
        files << "[#{@installer_var}]: #{user_installer}" if user_installer

        selection = 0

        # For the fantastically lazy, allow an environment variable to specify
        # which package selection to use. Special value of '-1' or 'installer' will
        # use the INSTALLER env var automatically (instead of just putting it in
        # the list to choose from).
        if ENV.has_key? @autopackage_var

            selection = ENV[@autopackage_var]
            if (selection == 'installer' or selection == '-1') and user_installer
                # Auto pick the INSTALLER pacckage
                selection = files.length
            else
                selection = selection.to_i
            end

            if selection <= 0 or selection > files.length
                puts "Invalid #{@autopackage_var} selection of #{selection}."
                selection = get_selection(files)
            else
                puts "Using #{@autopackage_var} selection of #{files[selection - 1]}"
            end

        else
            selection = get_selection(files)
        end

        if selection == files.length  and user_installer
            user_installer # we munged the text on this one
        else
            files[selection - 1]
        end

    end

    def get_selection(files)
        selection = 0
        files.each_index do |x|
            puts " #{x+1}) #{files[x]}\n"
        end
        loop do
            print "Select an image, or set the #{@installer_var} variable and run again: [1 - #{files.length}]: "
            selection = $stdin.gets.chomp.to_i
            break if selection > 0 and selection <= files.length
        end
        selection
    end

    def base_path
        File.absolute_path(File.join(Dir.pwd, "../"))
    end
end

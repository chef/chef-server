name "private-chef-cookbooks"

dependency "berkshelf"

project_name = project.name

source :path => File.expand_path("files/#{project_name}-cookbooks/#{project_name}", Omnibus.project_root)

build do
  gem "install uuidtools --no-rdoc --no-ri -v 2.1.3"
  command "mkdir -p #{install_dir}/embedded/cookbooks"
  command "#{install_dir}/bin/berks install -c ./Berksfile --path=#{install_dir}/embedded/cookbooks",
          :env => { "RUBYOPT"         => nil,
                    "BUNDLE_BIN_PATH" => nil,
                    "BUNDLE_GEMFILE"  => nil,
                    "GEM_PATH"        => nil,
                    "GEM_HOME"        => nil }
  block do
    File.open("#{install_dir}/embedded/cookbooks/dna.json", "w") do |f|
      f.puts "{\"run_list\": [ \"recipe[#{project_name}]\" ]}"
    end
    File.open("#{install_dir}/embedded/cookbooks/show-config.json", "w") do |f|
      f.puts "{\"run_list\": [ \"recipe[#{project_name}::show_config]\" ]}"
    end
    File.open("#{install_dir}/embedded/cookbooks/solo.rb", "w") do |f|
      f.puts "CURRENT_PATH = File.expand_path(File.dirname(__FILE__))"
      f.puts "file_cache_path \"\#\{CURRENT_PATH\}/cache\""
      f.puts "cookbook_path CURRENT_PATH"
      f.puts "verbose_logging true"
    end
  end
end

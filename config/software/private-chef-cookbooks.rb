name "private-chef-cookbooks"

dependency "berkshelf2"

project_name = "private-chef"

source :path => File.expand_path("files/#{project_name}-cookbooks", Config.project_root)

build do
  gem "install uuidtools --no-rdoc --no-ri -v 2.1.3"
  command "mkdir -p #{install_dir}/embedded/cookbooks"
  command "#{install_dir}/embedded/bin/berks install --berksfile=./#{project_name}/Berksfile --path=#{install_dir}/embedded/cookbooks"
  block do
    File.open("#{install_dir}/embedded/cookbooks/dna.json", "w") do |f|
      f.puts "{\"run_list\": [ \"recipe[#{project_name}]\" ]}"
    end
    File.open("#{install_dir}/embedded/cookbooks/show-config.json", "w") do |f|
      f.puts "{\"run_list\": [ \"recipe[#{project_name}::show_config]\" ]}"
    end
    File.open("#{install_dir}/embedded/cookbooks/post_upgrade_cleanup.json", "w") do |f|
      f.puts <<JSON
{"run_list": ["recipe[#{project_name}::post_11_upgrade_cleanup]",
              "recipe[#{project_name}::post_12_upgrade_cleanup]"]}
JSON
    end
    File.open("#{install_dir}/embedded/cookbooks/solo.rb", "w") do |f|
      f.puts "CURRENT_PATH = File.expand_path(File.dirname(__FILE__))"
      f.puts "file_cache_path \"\#\{CURRENT_PATH\}/cache\""
      f.puts "cookbook_path CURRENT_PATH"
      f.puts "verbose_logging true"
    end
  end
end

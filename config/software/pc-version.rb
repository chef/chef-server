name "pc-version"
description "generates a version manifest file"

build do
  block do
    File.open("#{install_dir}/pc-version.txt", "w") do |f|
      f.puts "Opscode Private Chef #{Omnibus::BuildVersion.full}"
      f.puts ""
      f.puts Omnibus::Reports.pretty_version_map(project)
    end
  end
end

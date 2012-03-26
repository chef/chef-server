name "yajl"
version "1.1.0"

dependencies ["rubygems"]

relative_path "yajl-ruby"

if platform == "solaris2"
  source :git => "git://github.com/Atalanta/yajl-ruby", :branch => "sparc"

  build do
    gem "build yajl-ruby.gemspec"
    gem ["install yajl-ruby-#{version}.gem",
         "-n #{install_dir}/bin",
         "--no-rdoc --no-ri"].join(" ")
  end
else
  build do
    gem ["install yajl-ruby",
         "-v #{version}",
         "-n #{install_dir}/bin",
         "--no-rdoc --no-ri"].join(" ")
  end
end

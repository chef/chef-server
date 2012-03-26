name "yajl"
gem_version = "1.1.0"

dependencies ["rubygems"]

relative_path "yajl-ruby"

if platform == "solaris2"
  version "sparc"
  source :git => "git://github.com/Atalanta/yajl-ruby"

  build do
    gem "build yajl-ruby.gemspec"
    gem ["install yajl-ruby-#{gem_version}.gem",
         "-n #{install_dir}/bin",
         "--no-rdoc --no-ri"].join(" ")
  end
else
  version "1.1.0"
  build do
    gem ["install yajl-ruby",
         "-v #{gem_version}",
         "-n #{install_dir}/bin",
         "--no-rdoc --no-ri"].join(" ")
  end
end

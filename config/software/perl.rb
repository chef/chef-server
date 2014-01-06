name "perl"
version "5.18.0"

source :url => "http://www.cpan.org/src/5.0/perl-#{version}.tar.gz",
       :md5 => "197ce31e84936bc0a83b03b2ee714cff"

relative_path "perl-#{version}"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "CFLAGS" => "-O3 -g -pipe -I#{install_dir}/embedded/include -L#{install_dir}/embedded/lib",
  "LDFLAGS" => "-Wl,-rpath,#{install_dir}/embedded/lib -L#{install_dir}/embedded/lib"
}

build do
  command "./configure.gnu --prefix=#{install_dir}/embedded", :env => env
  command "make", :env => env
  command "make install", :env => env

  # Ensure we have a sane omnibus-friendly CPAN config. This should be passed
  # to cpan any commands with the `-j` option.
  omnibus_cpan_home = File.join(cache_dir, 'cpan')
  command "mkdir -p #{omnibus_cpan_home}", :env => env
  block do
    open("#{omnibus_cpan_home}/OmnibusConfig.pm", "w") do |file|
      file.print <<-EOH

$CPAN::Config = {
  'build_dir' => q[#{omnibus_cpan_home}/build],
  'cpan_home' => q[#{omnibus_cpan_home}],
  'histfile' => q[#{omnibus_cpan_home}/histfile],
  'keep_source_where' => q[#{omnibus_cpan_home}/sources],
  'prefs_dir' => q[#{omnibus_cpan_home}/prefs],
  'urllist' => [q[http://cpan.llarian.net/], q[http://cpan.mirror.vexxhost.com/], q[http://noodle.portalus.net/CPAN/]],
};
1;
__END__
       EOH
    end
  end
end

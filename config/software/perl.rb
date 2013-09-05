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
end

name "automake"
version "1.11.2"

dependencies ["autoconf"]

source :url => "http://ftp.gnu.org/gnu/automake/automake-1.11.2.tar.gz",
       :md5 => "79ad64a9f6e83ea98d6964cef8d8a0bc"

relative_path "automake-1.11.2"

configure_env = {
  "LDFLAGS" => "-R#{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "PATH" => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

build do
  command "./bootstrap", :env => {"PATH" => "#{install_dir}/embedded/bin:#{ENV['PATH']}"}
  command "./configure --prefix=#{install_dir}/embedded", :env => configure_env
  command "make -j #{max_build_jobs}"
  command "make install"
end

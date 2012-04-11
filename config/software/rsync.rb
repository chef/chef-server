name "rsync"
version "3.0.9"

dependencies ["popt"]

source :url => "http://rsync.samba.org/ftp/rsync/src/rsync-3.0.9.tar.gz",
       :md5 => "5ee72266fe2c1822333c407e1761b92b"

relative_path "rsync-3.0.9"

env =
  case platform
  when "solaris2"
    {
      "LDFLAGS" => "-R #{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
    }
  else
    {
      "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -static-libgcc",
      "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
    }
  end

build do
  command "./configure --prefix=#{install_dir}/embedded --disable-iconv", :env => env
  command "make -j #{max_build_jobs}", :env => env
  command "make install"
end

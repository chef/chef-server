name "rsync"
version "3.0.9"

source :url => "http://rsync.samba.org/ftp/rsync/src/rsync-3.0.9.tar.gz",
       :md5 => "5ee72266fe2c1822333c407e1761b92b"

relative_path "rsync-3.0.9"

env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -static-libgcc",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "./configure --prefix=#{install_dir}/embedded --with-include-popt --disable-iconv", :env => env
  command "make -j #{max_build_jobs}", :env => env
  command "make install"
end

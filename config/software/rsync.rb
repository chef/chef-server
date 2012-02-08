name "rsync"

source :url => "http://rsync.samba.org/ftp/rsync/src/rsync-3.0.9.tar.gz",
       :md5 => "5ee72266fe2c1822333c407e1761b92b"

relative_path "rsync-3.0.9"

env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include -static-libgcc",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
}

build do
  command "./configure --prefix=/opt/opscode/embedded --with-include-popt --disable-iconv", :env => env
  command "make", :env => env
  command "make install"
end

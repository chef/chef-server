name "pcre"
version "8.30"

dependencies ["readline", "ncurses"]

source :url => "ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.30.tar.gz",
       :md5 => "d5ee0d9f6d2f0b7489331d04b6c182ef"

relative_path "pcre-8.30"

build do
  command("./configure --prefix=#{install_dir}/embedded",
          :env => {
            "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include"
          })
  # command "touch alocal.m4"
  command("make -j #{max_build_jobs}",
          :env => {
            "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}"
          })
  command "make install"
end

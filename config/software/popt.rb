name "popt"
version "1.16"

source :url => "http://rpm5.org/files/popt/popt-1.16.tar.gz",
       :md5 => "3743beefa3dd6247a73f8f7a32c14c33"

relative_path "popt-1.16"

env =
  case platform
  when "solaris2"
    {
      "LDFLAGS" => "-R#{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
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
  command "./configure --prefix=#{install_dir}/embedded", :env => env
  command "make -j #{max_build_jobs}", :env => env
  command "make install"
end



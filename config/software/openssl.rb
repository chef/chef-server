name "openssl"
version "1.0.0i"

dependencies ["zlib"]

source :url => "http://www.openssl.org/source/openssl-#{version}.tar.gz",
       :md5 => "b4df9c11af454fd68178c85a1d5f328f"

relative_path "openssl-#{version}"

build do
  # configure
  if platform == "darwin"
    command ["./Configure",
             "darwin-x86_64-cc",
             "--prefix=#{install_dir}/embedded",
             "--with-zlib-lib=#{install_dir}/embedded/lib",
             "--with-zlib-include=#{install_dir}/embedded/include",
             "zlib",
             "shared"].join(" ")
  elsif platform == "solaris2"
    command ["./Configure",
             "solaris-sparcv9-cc",
             "--prefix=#{install_dir}/embedded",
             "--with-zlib-lib=#{install_dir}/embedded/lib",
             "--with-zlib-include=#{install_dir}/embedded/include",
             "zlib",
             "shared",
             "-L#{install_dir}/embedded/lib",
             "-I#{install_dir}/embedded/include",
             "-R#{install_dir}/embedded/lib"].join(" ")
  else
    command(["./config",
             "--prefix=#{install_dir}/embedded",
             "--with-zlib-lib=#{install_dir}/embedded/lib",
             "--with-zlib-include=#{install_dir}/embedded/include",
             "zlib",
             "shared",
             "-L#{install_dir}/embedded/lib",
             "-I#{install_dir}/embedded/include"].join(" "),
            :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"})
  end

  # make and install
  command "make", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "make install"
end

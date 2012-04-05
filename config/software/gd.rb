name "gd"
version "2.3.33"

dependencies ["libiconv", "zlib", "libjpeg", "libpng"]

# TODO: make sure that this is where we want to download libgd from
source :url => "https://bitbucket.org/pierrejoye/gd-libgd/get/GD_2_0_33.tar.gz",
       :md5 => "b707be46e4047d5cdcf29af76b6e99bc"

relative_path "pierrejoye-gd-libgd-5551f61978e3"

source_dir = "#{project_dir}/src"

configure_env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib",
  "LIBS" => "-liconv"
}

build do
  command(["./configure",
           "--prefix=#{install_dir}/embedded",
           "--with-libiconv-prefix=#{install_dir}/embedded",
           "--with-jpeg=#{install_dir}/embedded",
           "--with-png=#{install_dir}/embedded",
           "--without-x", "--without-freetype",
           "--without-fontconfig",
           "--without-xpm"].join(" "),
          :env => configure_env,
          :cwd => source_dir)

  command "make -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/bin", "LIBS" => "-liconv"}, :cwd => source_dir
  command "make install", :cwd => source_dir
end

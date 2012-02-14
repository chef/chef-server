name "gd"

dependencies ["libiconv", "zlib", "libjpeg", "libpng"]

# TODO: make sure that this is where we want to download libgd from
source :url => "https://bitbucket.org/pierrejoye/gd-libgd/get/GD_2_0_33.tar.gz",
       :md5 => "b707be46e4047d5cdcf29af76b6e99b"

relative_path "pierrejoye-gd-libgd-5551f61978e3"

source_dir = "#{project_dir}/src"

configure_env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib",
  "LIBS" => "-liconv"
}

build do
  command(["./configure",
           "--prefix=/opt/opscode/embedded",
           "--with-libiconv-prefix=/opt/opscode/embedded",
           "--with-jpeg=/opt/opscode/embedded",
           "--with-png=/opt/opscode/embedded",
           "--without-x" "--without-freetype",
           "--without-fontconfig",
           "--without-xpm"].join(" "),
          :env => configure_env,
          :cwd => source_dir)

  command "make", :env => {"LD_RUN_PATH" => "/opt/opsocde/embedded/bin", "LIBS" => "-liconv"}, :cwd => source_dir
  command "make install", :cwd => source_dir
end

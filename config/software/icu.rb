name "icu"
version "4.8.1.1"

source :url => "http://download.icu-project.org/files/icu4c/4.8.1.1/icu4c-4_8_1_1-src.tgz",
:md5 => "ea93970a0275be6b42f56953cd332c17"

relative_path "icu"

working_dir = "#{project_dir}/source"

build do
  command("./configure --prefix=/opt/opscode/embedded",
          :env => {
            "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include"
          },
          :cwd => working_dir)
  command("make",
          :env => {
            "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
          },
          :cwd => working_dir)
  command "make install", :cwd => working_dir
end

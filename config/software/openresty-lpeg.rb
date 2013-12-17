name "openresty-lpeg"
version "0.12"

dependency "openresty"

source :url => "http://www.inf.puc-rio.br/~roberto/lpeg/lpeg-#{version}.tar.gz",
       :md5 => "4abb3c28cd8b6565c6a65e88f06c9162"

relative_path "lpeg-#{version}"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
}

lua_dir = "#{install_dir}/embedded/luajit/include/luajit-2.0"

build do
  command "make LUADIR=#{lua_dir}", :env => env
  command "install -p -m 0755 lpeg.so #{install_dir}/embedded/lualib"
end
